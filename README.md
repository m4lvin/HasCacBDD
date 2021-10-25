HasCacBDD
=========

[![Release](https://img.shields.io/github/release/m4lvin/HasCacBDD.svg)](https://github.com/m4lvin/HasCacBDD/releases)
[![Hackage](https://img.shields.io/hackage/v/HasCacBDD.svg)](https://hackage.haskell.org/package/HasCacBDD)

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

3. Play :-)

       λ> import Data.HasCacBDD
       λ> var 5
       Var 5 Top Bot
       λ> neg (var 5)
       Var 5 Bot Top
       λ> dis (neg (var 3)) (var 3)
       Top
