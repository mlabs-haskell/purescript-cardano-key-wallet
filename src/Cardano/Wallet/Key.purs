module Cardano.Wallet.Key
  ( KeyWallet(KeyWallet)
  , PrivateDrepKey(PrivateDrepKey)
  , PrivatePaymentKey(PrivatePaymentKey)
  , PrivateStakeKey(PrivateStakeKey)
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
import Cardano.Types.Address (Address(BaseAddress, EnterpriseAddress))
import Cardano.Types.Coin (Coin)
import Cardano.Types.Credential (Credential(PubKeyHashCredential))
import Cardano.Types.NetworkId (NetworkId)
import Cardano.Types.PaymentCredential (PaymentCredential(PaymentCredential))
import Cardano.Types.PrivateKey (PrivateKey(PrivateKey))
import Cardano.Types.PrivateKey as PrivateKey
import Cardano.Types.PublicKey as PublicKey
import Cardano.Types.RawBytes (RawBytes)
import Cardano.Types.StakeCredential (StakeCredential(StakeCredential))
import Cardano.Types.Transaction (Transaction, hash)
import Cardano.Types.TransactionUnspentOutput (TransactionUnspentOutput)
import Cardano.Types.TransactionWitnessSet
  ( TransactionWitnessSet(TransactionWitnessSet)
  )
import Cardano.Types.UtxoMap (UtxoMap)
import Data.Array (fromFoldable)
import Data.Either (note)
import Data.Foldable (fold)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (class Newtype, unwrap, wrap)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)

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
  , signData :: NetworkId -> RawBytes -> Aff DataSignature
  , paymentKey :: Aff PrivatePaymentKey
  , stakeKey :: Aff (Maybe PrivateStakeKey)
  , drepKey :: Aff PrivateDrepKey
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

getPrivatePaymentKey :: KeyWallet -> Aff PrivatePaymentKey
getPrivatePaymentKey = unwrap >>> _.paymentKey

getPrivateStakeKey :: KeyWallet -> Aff (Maybe PrivateStakeKey)
getPrivateStakeKey = unwrap >>> _.stakeKey

getPrivateDrepKey :: KeyWallet -> Aff PrivateDrepKey
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
  -> PrivateDrepKey
  -> KeyWallet
privateKeysToKeyWallet payKey mbStakeKey drepKey =
  KeyWallet
    { address
    , selectCollateral
    , signTx
    , signData
    , paymentKey: pure payKey
    , stakeKey: pure mbStakeKey
    , drepKey: pure drepKey
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

  -- TODO: Sign tx with the DRep key when required
  signTx :: Transaction -> Aff TransactionWitnessSet
  signTx tx = liftEffect do
    let
      txHash = hash tx
      payWitness = PrivateKey.makeVkeyWitness txHash (unwrap payKey)
      mbStakeWitness =
        mbStakeKey <#> \stakeKey ->
          PrivateKey.makeVkeyWitness txHash (unwrap stakeKey)
      witnessSet' =
        updateVkeys ([ payWitness ] <> fold (pure <$> mbStakeWitness)) mempty
    pure witnessSet'
    where
    updateVkeys
      :: Array Vkeywitness
      -> TransactionWitnessSet
      -> TransactionWitnessSet
    updateVkeys newVkeys (TransactionWitnessSet tws) =
      TransactionWitnessSet (tws { vkeys = newVkeys })

  -- TODO: Support signing data using DRep keys
  signData :: NetworkId -> RawBytes -> Aff DataSignature
  signData networkId payload = do
    addr <- address networkId
    liftEffect $ MessageSigning.signData (unwrap payKey) addr payload
