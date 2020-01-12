#!/usr/bin/env bash
cabal sandbox delete
cabal sandbox init


cabal install QuickCheck hspec regex-base regex-pcre stringsearch text tagged