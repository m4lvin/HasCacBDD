# HasCacBDD Changelog

## upcoming

- ...

## v0.3.0.0 (2025-01-01)

- add `optimalOrder`
- fix segfault on Apple M1 and ARM aarch64
- minor improvements of documentation and tests

## v0.2.0.0 (2023-11-23)

- rename `forall` (soon a keyword in GHC) to `forall_` and `exists` to `exists_`

## v0.1.0.4 (2023-02-03)

Maintenance release.

- Faster subsOf and sizeOf.

## v0.1.0.3 (2020-06-30)

Maintenance release.

- fix an error which caused `svgGraph` to hang on long outputs.
- add `Read` and `Eq` classes for BddTree.
- a few more tests.

## v0.1.0.2 (2019-06-19)

Correction to extra library paths defined in custom `Setup.hs`. This should fix problems when HasCacBDD is used as a dependency in projects with recent stack versions.

## v0.1.0.1 (2019-01-24)

- new functions: `relabelFun`, `substit`, `substitSimul`
- simplified test suite
- improve documentation and hints to get started

## v0.1.0.0 (2017-03-09)

First complete release, also available on hackage.
