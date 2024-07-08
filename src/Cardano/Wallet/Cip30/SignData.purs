module Cardano.Wallet.Cip30.SignData
  ( COSEKey
  , COSESign1
  ) where

import Cardano.Types (CborBytes, PublicKey, RawBytes)
import Data.Maybe (Maybe)
import Effect (Effect)

foreign import data COSESign1 :: Type
foreign import _getCoseSign1ProtectedHeaderAlg
  :: MaybeFfiHelper -> COSESign1 -> Maybe Int

foreign import _getCoseSign1ProtectedHeaderAddress
  :: MaybeFfiHelper -> COSESign1 -> Maybe CborBytes

foreign import _getCoseSign1ProtectedHeaderKid
  :: MaybeFfiHelper -> COSESign1 -> Maybe RawBytes

foreign import data COSEKey :: Type
foreign import _getCoseKeyHeaderKty :: MaybeFfiHelper -> COSEKey -> Maybe Int
foreign import _getCoseKeyHeaderAlg :: MaybeFfiHelper -> COSEKey -> Maybe Int
foreign import _getCoseKeyHeaderCrv :: MaybeFfiHelper -> COSEKey -> Maybe Int
foreign import _getCoseKeyHeaderX :: MaybeFfiHelper -> COSEKey -> Maybe RawBytes
foreign import _getCoseKeyHeaderKid
  :: MaybeFfiHelper -> COSEKey -> Maybe RawBytes

foreign import fromBytesCoseSign1 :: CborBytes -> Effect COSESign1
foreign import fromBytesCoseKey :: CborBytes -> Effect COSEKey

foreign import getSignedData :: COSESign1 -> Effect CborBytes
foreign import verifySignature
  :: COSESign1 -> PublicKey -> CborBytes -> Effect Boolean

-- Helpers

type MaybeFfiHelper =
  { nothing :: forall (x :: Type). Maybe x
  , just :: forall (x :: Type). x -> Maybe x
  , from :: forall (x :: Type). x -> Maybe x -> x
  }

