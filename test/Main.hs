module Main where

import qualified TestRegex.Format as F
import qualified TestRegex.Pcre as P
import qualified TestRegex.Replace as R
import qualified TestRegex.StringSearch as S
import qualified TestRegex.TestTrim as T



main::IO()
main = do
            F.main
            P.main
            R.main
            S.main
            T.main