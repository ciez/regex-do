module Main where

import qualified TestRegex.TestFormat as F
import qualified TestRegex.TestPcre as P
import qualified TestRegex.TestReplace as R
import qualified TestRegex.TestReplaceOpen as O
import qualified TestRegex.TestSplit as S
import qualified TestRegex.TestTrim as T



main::IO()
main = do
            F.main
            P.main
            R.main
            S.main
            T.main
            O.main