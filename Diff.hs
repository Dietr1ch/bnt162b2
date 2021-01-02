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

isWrong :: RNACodonDiff -> Bool
isWrong d = from (a d) /= from (b d)


data RNAFastaDiff = RNAFastaDiff { l :: T.Text
                                 , r :: T.Text
                                 , codonDiffs :: [RNACodonDiff]
                                 } deriving (Eq, Show, Read)

codonZip :: RNAFasta -> RNAFasta -> [(RNACodon, RNACodon)]
codonZip l r = zip (codons l) (codons r)

diff :: RNAFasta -> RNAFasta -> RNAFastaDiff
diff l r = RNAFastaDiff { l = header l
                        , r = header r
                        , codonDiffs = map (fromPair) (codonZip l r)
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

sizeCompare :: Int -> Int -> String
sizeCompare n total = (show n) ++ " (" ++ (show percent) ++ "%)"
                      where
                        percent = (100 * n  `div` total)

dShow :: RNAFastaDiff -> String
dShow d = intercalate "\n" [ "Left:   " ++ (show hLeft)
                           , "Right:  " ++ (show hRight)
                           , ""
                           , "Same:   " ++ (sizeCompare same  total)
                           , "Equiv:  " ++ (sizeCompare equiv total)
                           , "Equiv': " ++ (sizeCompare delta total)
                           , "Wrong:  " ++ (sizeCompare wrong total)
                           , "Total:  " ++ (show total)
                           , ""
                           , (cmpDiffs (codonDiffs d))]
           where
             hLeft  = l d
             hRight = r d
             cDiffs = codonDiffs d
             same  = length (filter isEq    cDiffs)
             equiv = length (filter isEquiv cDiffs)
             delta = equiv - same
             wrong = length (filter isWrong cDiffs)
             total = length cDiffs
