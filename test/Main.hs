module Main where

import qualified TestRegex.TestFormat as F
import qualified TestRegex.TestPcre as P
import qualified TestRegex.TestReplace as R
import qualified TestRegex.TestReplaceUtf as U
import qualified TestRegex.TestReplaceOpen as O
import qualified TestRegex.TestSplit as S
import qualified TestRegex.TestTrim as T
import qualified TestRegex.TestMakeRegexM as E

main::IO()
main = do
            F.main
            P.main
            S.main
            T.main
            O.main
            R.main
            U.main
            E.main
