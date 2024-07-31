module Cardano.Wallet.Key
  ( KeyWallet(KeyWallet)
  , PrivateDrepKey(PrivateDrepKey)
  , PrivatePaymentKey(PrivatePaymentKey)
  , PrivateStakeKey(PrivateStakeKey)
  , privateKeyToPkh
  , privateKeysToAddress
  , privateKeysToKeyWallet
  , getPrivateDrepKey
  , getPrivatePaymentKey
  , getPrivateStakeKey
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , JsonDecodeError(TypeMismatch)
  , decodeAeson
  , encodeAeson
  )
import Cardano.Collateral.Select as Collateral
import Cardano.MessageSigning (DataSignature)
import Cardano.MessageSigning (signData) as MessageSigning
import Cardano.Types (Vkeywitness)
import Cardano.Types.Address
  ( Address(BaseAddress, EnterpriseAddress, RewardAddress)
  , mkPaymentAddress
  )
import Cardano.Types.Certificate
  ( Certificate(RegDrepCert, UnregDrepCert, UpdateDrepCert)
  )
import Cardano.Types.Coin (Coin)
import Cardano.Types.Credential (Credential(PubKeyHashCredential))
import Cardano.Types.Ed25519KeyHash (Ed25519KeyHash)
import Cardano.Types.NetworkId (NetworkId)
import Cardano.Types.PaymentCredential (PaymentCredential(PaymentCredential))
import Cardano.Types.PrivateKey (PrivateKey(PrivateKey))
import Cardano.Types.PrivateKey as PrivateKey
import Cardano.Types.PublicKey as PublicKey
import Cardano.Types.RawBytes (RawBytes)
import Cardano.Types.StakeCredential (StakeCredential(StakeCredential))
import Cardano.Types.Transaction (Transaction(Transaction), hash)
import Cardano.Types.TransactionBody (TransactionBody(TransactionBody))
import Cardano.Types.TransactionUnspentOutput (TransactionUnspentOutput)
import Cardano.Types.TransactionWitnessSet
  ( TransactionWitnessSet(TransactionWitnessSet)
  )
import Cardano.Types.UtxoMap (UtxoMap)
import Cardano.Types.Voter (Voter(Drep))
import Control.Monad.Error.Class (liftMaybe)
import Data.Array (catMaybes, elem) as Array
import Data.Array (fromFoldable)
import Data.Either (note)
import Data.Foldable (any)
import Data.Generic.Rep (class Generic)
import Data.Map (member) as Map
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (error)

-------------------------------------------------------------------------------
-- Key backend
-------------------------------------------------------------------------------

-- | An interface that wraps `PrivateKey`s. Used in CTL.
-- | Technically, can be implemented with remote calls, e.g. over HTTP,
-- | to provide signing services without revealing the private key.
newtype KeyWallet = KeyWallet
  { address :: NetworkId -> Aff Address
  , selectCollateral ::
      Coin
      -- ^ Minimum required collateral
      -> Coin
      -- ^ Lovelace per UTxO byte parameter
      -> Int
      -- ^ Maximum number of collateral inputs (use 3)
      -> UtxoMap
      -- ^ UTxOs to select from
      -> Aff (Maybe (Array TransactionUnspentOutput))
  , signTx :: Transaction -> Aff TransactionWitnessSet
  , signData :: Address -> RawBytes -> Aff (Maybe DataSignature)
  , paymentKey :: Aff PrivatePaymentKey
  , stakeKey :: Aff (Maybe PrivateStakeKey)
  , drepKey :: Aff (Maybe PrivateDrepKey)
  }

derive instance Newtype KeyWallet _

newtype PrivatePaymentKey = PrivatePaymentKey PrivateKey

derive instance Newtype PrivatePaymentKey _

instance Show PrivatePaymentKey where
  show _ = "(PrivatePaymentKey <hidden>)"

instance EncodeAeson PrivatePaymentKey where
  encodeAeson (PrivatePaymentKey pk) = encodeAeson
    (PrivateKey.toBech32 pk)

instance DecodeAeson PrivatePaymentKey where
  decodeAeson aeson =
    decodeAeson aeson >>=
      note (TypeMismatch "PrivateKey")
        <<< map PrivatePaymentKey
        <<< PrivateKey.fromBech32

newtype PrivateStakeKey = PrivateStakeKey PrivateKey

derive instance Newtype PrivateStakeKey _

instance Show PrivateStakeKey where
  show _ = "(PrivateStakeKey <hidden>)"

instance EncodeAeson PrivateStakeKey where
  encodeAeson (PrivateStakeKey pk) = encodeAeson
    (PrivateKey.toBech32 pk)

instance DecodeAeson PrivateStakeKey where
  decodeAeson aeson =
    decodeAeson aeson >>=
      note (TypeMismatch "PrivateKey")
        <<< map PrivateStakeKey
        <<< PrivateKey.fromBech32

newtype PrivateDrepKey = PrivateDrepKey PrivateKey

derive instance Newtype PrivateDrepKey _

instance Show PrivateDrepKey where
  show _ = "(PrivateDrepKey <hidden>)"

instance EncodeAeson PrivateDrepKey where
  encodeAeson (PrivateDrepKey pk) = encodeAeson
    (PrivateKey.toBech32 pk)

instance DecodeAeson PrivateDrepKey where
  decodeAeson aeson =
    decodeAeson aeson >>=
      note (TypeMismatch "PrivateKey")
        <<< map PrivateDrepKey
        <<< PrivateKey.fromBech32

privateKeyToPkh :: forall t. Newtype t PrivateKey => t -> Ed25519KeyHash
privateKeyToPkh =
  PublicKey.hash
    <<< PrivateKey.toPublicKey
    <<< unwrap

privateKeyToPkhCred :: forall t. Newtype t PrivateKey => t -> Credential
privateKeyToPkhCred = PubKeyHashCredential <<< privateKeyToPkh

getPrivatePaymentKey :: KeyWallet -> Aff PrivatePaymentKey
getPrivatePaymentKey = unwrap >>> _.paymentKey

getPrivateStakeKey :: KeyWallet -> Aff (Maybe PrivateStakeKey)
getPrivateStakeKey = unwrap >>> _.stakeKey

getPrivateDrepKey :: KeyWallet -> Aff (Maybe PrivateDrepKey)
getPrivateDrepKey = unwrap >>> _.drepKey

privateKeysToAddress
  :: PrivatePaymentKey -> Maybe PrivateStakeKey -> NetworkId -> Address
privateKeysToAddress payKey mbStakeKey networkId = do
  let pubPayKey = PrivateKey.toPublicKey (unwrap payKey)
  case mbStakeKey of
    Just stakeKey ->
      let
        pubStakeKey = PrivateKey.toPublicKey (unwrap stakeKey)
      in
        BaseAddress
          { networkId
          , paymentCredential:
              ( PaymentCredential $ PubKeyHashCredential $ PublicKey.hash $
                  pubPayKey
              )
          , stakeCredential:
              ( StakeCredential $ PubKeyHashCredential $ PublicKey.hash $
                  pubStakeKey
              )
          }

    Nothing -> pubPayKey # PublicKey.hash
      >>> PubKeyHashCredential
      >>> wrap
      >>> { networkId, paymentCredential: _ }
      >>> EnterpriseAddress

privateKeysToKeyWallet
  :: PrivatePaymentKey
  -> Maybe PrivateStakeKey
  -> Maybe PrivateDrepKey
  -> KeyWallet
privateKeysToKeyWallet payKey mbStakeKey mbDrepKey =
  KeyWallet
    { address
    , selectCollateral
    , signTx
    , signData
    , paymentKey: pure payKey
    , stakeKey: pure mbStakeKey
    , drepKey: pure mbDrepKey
    }
  where
  address :: NetworkId -> Aff Address
  address networkId = pure $ privateKeysToAddress payKey mbStakeKey networkId

  selectCollateral
    :: Coin
    -> Coin
    -> Int
    -> UtxoMap
    -> Aff (Maybe (Array TransactionUnspentOutput))
  selectCollateral
    minRequiredCollateral
    coinsPerUtxoByte
    maxCollateralInputs
    utxos = pure $ fromFoldable
    -- Use 5 ADA as the minimum required collateral.
    <$> Collateral.selectCollateral coinsPerUtxoByte maxCollateralInputs
      minRequiredCollateral
      utxos

  signTx :: Transaction -> Aff TransactionWitnessSet
  signTx tx@(Transaction { body: TransactionBody txBody }) = liftEffect do
    let
      txHash = hash tx
      payWitness = PrivateKey.makeVkeyWitness txHash (unwrap payKey)
      mbStakeWitness =
        mbStakeKey <#> \stakeKey ->
          PrivateKey.makeVkeyWitness txHash (unwrap stakeKey)
      mbDrepWitness = do
        drepKey <- mbDrepKey
        if drepSigRequired drepKey then Just $ PrivateKey.makeVkeyWitness txHash
          (unwrap drepKey)
        else Nothing
      witnessSet =
        updateVkeys
          (Array.catMaybes [ Just payWitness, mbStakeWitness, mbDrepWitness ])
          mempty
    pure witnessSet
    where
    updateVkeys
      :: Array Vkeywitness
      -> TransactionWitnessSet
      -> TransactionWitnessSet
    updateVkeys newVkeys (TransactionWitnessSet tws) =
      TransactionWitnessSet (tws { vkeys = newVkeys })

    drepSigRequired :: PrivateDrepKey -> Boolean
    drepSigRequired drepKey =
      checkCerts
        || checkVotes
        || checkRequiredSigners
      where
      checkCerts :: Boolean
      checkCerts = any isDrepCert txBody.certs

      checkVotes :: Boolean
      checkVotes = Map.member (Drep drepCred) $ unwrap txBody.votingProcedures

      checkRequiredSigners :: Boolean
      checkRequiredSigners = Array.elem drepPkh txBody.requiredSigners

      drepPkh :: Ed25519KeyHash
      drepPkh = privateKeyToPkh drepKey

      drepCred :: Credential
      drepCred = PubKeyHashCredential drepPkh

      isDrepCert :: Certificate -> Boolean
      isDrepCert = case _ of
        RegDrepCert cred _ _ -> cred == drepCred
        UnregDrepCert cred _ -> cred == drepCred
        UpdateDrepCert cred _ -> cred == drepCred
        _ -> false
  
  -- Inspect and provide a DataSignature for the supplied data using
  -- a key identified by the supplied address.
  --
  -- This function returns Nothing if the wallet does not have the
  -- required keys.
  --
  -- Supported Credentials:
  --   payment key: base addresses with any stake credential, enterprise addresses
  --   stake key: reward addresses
  --   drep key: enterprise addresses
  --
  -- NOTE: Pointer addresses are not supported.
  signData :: Address -> RawBytes -> Aff (Maybe DataSignature)
  signData addr payload =
    liftEffect do
      case addr of
        BaseAddress baseAddr
          | baseAddr.paymentCredential == payCred ->
              Just <$> MessageSigning.signData (unwrap payKey) addr payload

        RewardAddress rewardAddr
          | Just stakeKey@(PrivateStakeKey key) <- mbStakeKey
          , rewardAddr.stakeCredential == wrap (privateKeyToPkhCred stakeKey) ->
              Just <$> MessageSigning.signData key addr payload

        EnterpriseAddress entAddr
          | entAddr.paymentCredential == payCred ->
              Just <$> MessageSigning.signData (unwrap payKey) addr payload

        EnterpriseAddress entAddr
          | Just drepKey@(PrivateDrepKey key) <- mbDrepKey
          , entAddr.paymentCredential == wrap (privateKeyToPkhCred drepKey) ->
              Just <$> MessageSigning.signData key addr payload

        _ -> pure Nothing
    where
    payCred :: PaymentCredential
    payCred = wrap $ privateKeyToPkhCred payKey
