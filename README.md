# purescript-cardano-key-wallet

Provides a common interface for wallet operations:

```purescript
newtype KeyWallet = KeyWallet
  { address :: NetworkId -> Aff Address
  , selectCollateral ::
      Coin
      -> Int
      -> UtxoMap
      -> Aff (Maybe (Array TransactionUnspentOutput))
  , signTx :: Transaction -> Aff TransactionWitnessSet
  , signData :: NetworkId -> RawBytes -> Aff DataSignature
  , paymentKey :: Aff PrivatePaymentKey
  , stakeKey :: Aff (Maybe PrivateStakeKey)
  }
```

This interface is used in [`cardano-transaction-lib`](https://github.com/Plutonomicon/cardano-transaction-lib) to encapsulate private keys.

It can be used to provide signing services remotely (note the `Aff`) without revealing the private keys.

To create a `KeyWallet` from private key(s), use this function:

```purescript
Cardano.Wallet.Key.privateKeysToKeyWallet
  :: PrivatePaymentKey -> Maybe PrivateStakeKey -> KeyWallet
```

## Test coverage

CTL's test suite uses `KeyWallet` to run every contract.
