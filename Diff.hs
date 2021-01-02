module Diff ( diff
            , dShow
            ) where

import Data.List (intercalate)
import qualified Data.Text as T

import Bio ( RNANucleobase(..)
           , RNACodon(..)
           , RNAAminoAcid(..)
           , RNAFasta(..)
           , from
           )

data RNACodonDiff = RNACodonDiff { a :: RNACodon
                                 , b :: RNACodon
                                 } deriving (Eq, Show, Read)

fromPair :: (RNACodon, RNACodon) -> RNACodonDiff
fromPair (l, r) = RNACodonDiff {a=l, b=r}

isEq :: RNACodonDiff -> Bool
isEq d = a d == b d

isEquiv :: RNACodonDiff -> Bool
isEquiv d = from (a d) == from (b d)


data RNAFastaDiff = RNAFastaDiff { l :: T.Text
                                 , r :: T.Text
                                 , codonDiffs :: [RNACodonDiff]
                                 } deriving (Eq, Show, Read)

diff :: RNAFasta -> RNAFasta -> RNAFastaDiff
diff l r = RNAFastaDiff { l = header l
                        , r = header r
                        , codonDiffs = map (fromPair) (zip (codons l) (codons r))
                        }

cmp :: RNACodonDiff -> String
cmp d
  | isEq d    = "Same  " ++ (show aa)
  | isEquiv d = "Equiv " ++ left ++ " ~= " ++ right
  | otherwise = "!!!!! " ++ left ++ " /= " ++ right
  where
    aa  = (a d)
    bb  = (b d)
    left  = (show aa) ++ " (" ++ (show (from aa)) ++ ")"
    right = (show bb) ++ " (" ++ (show (from bb)) ++ ")"

cmpDiffs :: [RNACodonDiff] -> String
cmpDiffs diffs = intercalate "\n" (map cmp diffs)

dShow :: RNAFastaDiff -> String
dShow d = intercalate "\n" [ "Left:  " ++ (show (l d))
                           , "Right: " ++ (show (r d))
                           , ""
                           , (cmpDiffs (codonDiffs d))]
