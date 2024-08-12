# Changelog

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/)
and we follow [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->

- [v2.0.0](#v200)
  - [Added](#added)
  - [Changed](#changed)
  - [Removed](#removed)
- [v1.0.0](#v100)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

## v2.0.0

### Added

- KeyWallet can now be configured with an optional DRep key, enabling users to
participate in governance voting and issue DRep certificates.
([#3](https://github.com/mlabs-haskell/purescript-cardano-key-wallet/pull/3))

### Changed

- `signData` now accepts `Address` as the first parameter and returns
`Maybe DataSignature`. The supplied address is inspected, and the keys
associated with that address are used to generate a signature. Additionally,
KeyWallet now supports signing with the DRep key as specified in CIP-95.
**NOTE**: If the wallet does not have the required keys, `Nothing` is returned.
Also, pointer addresses are not supported.
([#3](https://github.com/mlabs-haskell/purescript-cardano-key-wallet/pull/3))

- The internal logic of signTx has been updated. The contents of the supplied
transaction are now inspected to determine if a DRep signature needs to be
attached. Note that the logic here differs from the one used with the stake
witness, where a stake key signature is always attached if the key is present.
([#3](https://github.com/mlabs-haskell/purescript-cardano-key-wallet/pull/3))

### Removed

- `Cardano.Wallet.Cip30.SignData` - unused module

## v1.0.0
