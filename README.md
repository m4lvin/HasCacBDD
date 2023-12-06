HasCacBDD
=========

[![Release](https://img.shields.io/github/release/m4lvin/HasCacBDD.svg)](https://github.com/m4lvin/HasCacBDD/releases)
[![Hackage](https://img.shields.io/hackage/v/HasCacBDD.svg)](https://hackage.haskell.org/package/HasCacBDD)
[![GitLab CI](https://gitlab.com/m4lvin/HasCacBDD/badges/main/pipeline.svg)](https://gitlab.com/m4lvin/HasCacBDD/-/pipelines)
[![Test Coverage](https://gitlab.com/m4lvin/HasCacBDD/badges/main/coverage.svg)](https://gitlab.com/m4lvin/HasCacBDD/-/jobs/artifacts/main/file/hpc/combined/all/hpc_index.html?job=test)

Haskell bindings for CacBDD, a Binary Decision Diagram (BDD) package with dynamic cache management.

Original C++ code from <http://kailesu.net/CacBDD> and a C wrapper are included.


# Getting Started

1. Install C compilers and stack if necessary:

       apt install build-essential git
       curl -sSL https://get.haskellstack.org/ | sh

2. Download, build and load the lastest version:

       git clone https://github.com/m4lvin/HasCacBDD.git
       cd HasCacBDD
       stack setup
       stack build
       stack exec ghci

    Note: `stack ghci` apparently does not work with the shared library.
    You really need `stack build` and then `stack exec ghci`.

    To use cabal instead of stack: `cabal build`, then `cabal exec ghci` and then `:set -package HasCacBDD`.

3. Now you can play with Boolean functions :-)

       λ> import Data.HasCacBDD
       λ> var 5
       Var 5 Top Bot
       λ> neg (var 5)
       Var 5 Bot Top
       λ> dis (neg (var 3)) (var 3)
       Top

For further documentation, see <https://hackage.haskell.org/package/HasCacBDD/docs/Data-HasCacBDD.html>
