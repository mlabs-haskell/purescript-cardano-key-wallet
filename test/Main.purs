module Test.Main where

import Prelude

import Cardano.Wallet.Cip30.SignData (suite)
import Data.Maybe (Maybe(Just))
import Data.Time.Duration (Milliseconds(Milliseconds))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Mote.TestPlanM (interpretWithConfig)
import Test.Spec.Runner (defaultConfig)

main :: Effect Unit
main = launchAff_ $
  interpretWithConfig
    defaultConfig { timeout = Just $ Milliseconds 30_000.0, exit = true }
    suite
