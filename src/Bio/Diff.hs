module Bio.Diff ( diff
                ) where

import Data.List (intercalate)
import qualified Data.Text as T

import Bio.Formatting (PrettyPrint(..))
import Bio.RNA ( Nucleobase(..)
               , Codon(..)
               , AminoAcid(..)
               , Fasta(..)
               , from
               )

data CodonDiff = CodonDiff { a :: Codon
                                 , b :: Codon
                                 } deriving (Eq, Show, Read)

fromPair :: (Codon, Codon) -> CodonDiff
fromPair (l, r) = CodonDiff {a=l, b=r}

isEq :: CodonDiff -> Bool
isEq d = a d == b d

isEquiv :: CodonDiff -> Bool
isEquiv d = from (a d) == from (b d)

isWrong :: CodonDiff -> Bool
isWrong d = from (a d) /= from (b d)


data FastaDiff = FastaDiff { l :: T.Text
                                 , r :: T.Text
                                 , codonDiffs :: [CodonDiff]
                                 } deriving (Eq, Show, Read)

codonZip :: Fasta -> Fasta -> [(Codon, Codon)]
codonZip l r = zip (codons l) (codons r)

diff :: Fasta -> Fasta -> FastaDiff
diff l r = FastaDiff { l = header l
                        , r = header r
                        , codonDiffs = map (fromPair) (codonZip l r)
                        }

cmp :: CodonDiff -> String
cmp d
  | isEq d    = "Same  " ++ (show aa)
  | isEquiv d = "Equiv " ++ left ++ " ~= " ++ right
  | otherwise = "!!!!! " ++ left ++ " /= " ++ right
  where
    aa  = (a d)
    bb  = (b d)
    left  = (show aa) ++ " (" ++ (show (from aa)) ++ ")"
    right = (show bb) ++ " (" ++ (show (from bb)) ++ ")"

cmpDiffs :: [CodonDiff] -> String
cmpDiffs diffs = intercalate "\n" (map cmp diffs)

sizeCompare :: Int -> Int -> String
sizeCompare n total = (show n) ++ " (" ++ (show percent) ++ "%)"
                      where
                        percent = (100 * n  `div` total)

instance PrettyPrint FastaDiff where
  pp d = intercalate "\n" [ "Left:   " ++ (show hLeft)
                             , "Right:  " ++ (show hRight)
                             , ""
                             , "Same:   " ++ (sizeCompare same  total)
                             , "Equiv:  " ++ (sizeCompare equiv total)
                             , "Equiv': " ++ (sizeCompare delta total)
                             , "Wrong:  " ++ (sizeCompare wrong total)
                             , "Total:  " ++ (show total)
                             , ""
                             , cmpDiffs cDiffs]
             where
               hLeft  = l d
               hRight = r d
               cDiffs = codonDiffs d
               same  = length (filter isEq    cDiffs)
               equiv = length (filter isEquiv cDiffs)
               delta = equiv - same
               wrong = length (filter isWrong cDiffs)
               total = length cDiffs
