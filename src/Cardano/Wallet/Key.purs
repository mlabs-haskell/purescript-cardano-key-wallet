module Cardano.Key.Wallet
  ( KeyWallet(KeyWallet)
  , PrivatePaymentKey(PrivatePaymentKey)
  , PrivateStakeKey(PrivateStakeKey)
  , privateKeysToAddress
  , privateKeysToKeyWallet
  , keyWalletPrivatePaymentKey
  , keyWalletPrivateStakeKey
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson, JsonDecodeError(TypeMismatch), decodeAeson, encodeAeson)
import Cardano.Collateral.Select as Collateral
import Cardano.MessageSigning (DataSignature)
import Cardano.MessageSigning (signData) as MessageSigning
import Cardano.Types.Address (Address(BaseAddress, EnterpriseAddress))
import Cardano.Types.BigNum as BigNum
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
import Cardano.Types.TransactionWitnessSet (TransactionWitnessSet)
import Cardano.Types.UtxoMap (UtxoMap)
import Cardano.Types.Vkeywitness (Vkeywitness)
import Data.Array (fromFoldable)
import Data.Either (note)
import Data.Foldable (fold)
import Data.Lens (set, Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (class Newtype, unwrap, wrap)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Type.Prelude (Proxy(Proxy))

minRequiredCollateral :: Coin
minRequiredCollateral = wrap $ BigNum.fromInt 5_000_000

-------------------------------------------------------------------------------
-- Key backend
-------------------------------------------------------------------------------

-- | A wrapper over `PrivateKey` that provides an interface for CTL
newtype KeyWallet = KeyWallet
  { address :: NetworkId -> Aff Address
  , selectCollateral ::
      Coin
      -> Int
      -> UtxoMap
      -> Aff (Maybe (Array TransactionUnspentOutput))
  , signTx :: Transaction -> Aff TransactionWitnessSet
  , signData :: NetworkId -> RawBytes -> Aff DataSignature
  , paymentKey :: PrivatePaymentKey
  , stakeKey :: Maybe PrivateStakeKey
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

keyWalletPrivatePaymentKey :: KeyWallet -> PrivatePaymentKey
keyWalletPrivatePaymentKey = unwrap >>> _.paymentKey

keyWalletPrivateStakeKey :: KeyWallet -> Maybe PrivateStakeKey
keyWalletPrivateStakeKey = unwrap >>> _.stakeKey

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
  :: PrivatePaymentKey -> Maybe PrivateStakeKey -> KeyWallet
privateKeysToKeyWallet payKey mbStakeKey =
  KeyWallet
    { address
    , selectCollateral
    , signTx
    , signData
    , paymentKey: payKey
    , stakeKey: mbStakeKey
    }
  where
  address :: NetworkId -> Aff Address
  address networkId = pure $ privateKeysToAddress payKey mbStakeKey networkId

  selectCollateral
    :: Coin
    -> Int
    -> UtxoMap
    -> Aff (Maybe (Array TransactionUnspentOutput))
  selectCollateral coinsPerUtxoByte maxCollateralInputs utxos = pure $ fromFoldable
    -- Use 5 ADA as the minimum required collateral.
    <$> Collateral.selectCollateral coinsPerUtxoByte maxCollateralInputs minRequiredCollateral utxos

  signTx :: Transaction -> Aff TransactionWitnessSet
  signTx tx = liftEffect do
    let
      txHash = hash tx
      payWitness = PrivateKey.makeVkeyWitness txHash (unwrap payKey)
      mbStakeWitness =
        mbStakeKey <#> \stakeKey ->
          PrivateKey.makeVkeyWitness txHash (unwrap stakeKey)
    let
      witnessSet' = set _vkeys
        ([ payWitness ] <> fold (pure <$> mbStakeWitness))
        mempty
    pure witnessSet'

  signData :: NetworkId -> RawBytes -> Aff DataSignature
  signData networkId payload = do
    addr <- address networkId
    liftEffect $ MessageSigning.signData (unwrap payKey) addr payload

_vkeys :: Lens' TransactionWitnessSet (Array Vkeywitness)
_vkeys = _Newtype <<< prop (Proxy :: Proxy "vkeys")
