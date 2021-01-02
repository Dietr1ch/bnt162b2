{-# LANGUAGE ScopedTypeVariables #-}

module Bio.Parser ( readFasta
                  ) where

import Data.Fasta.Text.Parse (parsecFasta)
import Data.Fasta.Text.Types (FastaSequence(..))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Maybe
import Text.Read

import Bio.RNA ( Nucleobase(..)
               , Codon(..)
               , AminoAcid(..)
               , Fasta(..)
               )

-- TODO(dietr1ch): Parse `FastaSequence` into `Maybe Fasta`
-- TODO(dietr1ch): Consider using an unboxed vector of strict `Codon`s
readRNA :: Char -> Nucleobase
readRNA 'A' = A
readRNA 'C' = C
readRNA 'G' = G
readRNA 'U' = U
readRNA 'T' = U  -- Is this ok?
readRNA _ = undefined

unsafeCodons :: [Nucleobase] -> [Codon]
unsafeCodons [] = []
unsafeCodons (a:b:c:ns) = (Codon a b c) : unsafeCodons ns
unsafeCodons _ = undefined

codonsNB :: [Nucleobase] -> Maybe [Codon]
codonsNB ns
  | (length ns) `mod` 3 /= 0 = Nothing
  | otherwise = Just (unsafeCodons ns)

parseRNA :: FastaSequence -> Maybe Fasta
parseRNA fasta = Just (Fasta { header=(fastaHeader fasta)
                                , codons=unsafeCodons (map readRNA (T.unpack (fastaSeq fasta)))})

readFasta :: FilePath -> IO (Maybe Fasta)
readFasta path = do
  raw :: T.Text <- TIO.readFile path
  return (parseRNA (head (parsecFasta raw)))
