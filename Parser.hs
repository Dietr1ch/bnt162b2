{-# LANGUAGE ScopedTypeVariables #-}

module Parser ( readFasta
              ) where

import Data.Fasta.Text.Parse (parsecFasta)
import Data.Fasta.Text.Types (FastaSequence(..))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Maybe
import Text.Read

import Bio ( RNANucleobase(..)
           , RNACodon(..)
           , RNAAminoAcid(..)
           , RNAFasta(..)
           )

-- TODO(dietr1ch): Parse `FastaSequence` into `Maybe RNAFasta`
-- TODO(dietr1ch): Consider using an unboxed vector of strict `RNACodon`s
readRNA :: Char -> RNANucleobase
readRNA 'A' = A
readRNA 'C' = C
readRNA 'G' = G
readRNA 'U' = U
readRNA 'T' = U  -- Is this ok?
readRNA _ = undefined

unsafeCodons :: [RNANucleobase] -> [RNACodon]
unsafeCodons [] = []
unsafeCodons (a:b:c:ns) = (RNACodon a b c) : unsafeCodons ns
unsafeCodons _ = undefined

codonsNB :: [RNANucleobase] -> Maybe [RNACodon]
codonsNB ns
  | (length ns) `mod` 3 /= 0 = Nothing
  | otherwise = Just (unsafeCodons ns)

parseRNA :: FastaSequence -> Maybe RNAFasta
parseRNA fasta = Just (RNAFasta { header=(fastaHeader fasta)
                                , codons=unsafeCodons (map readRNA (T.unpack (fastaSeq fasta)))})

readFasta :: FilePath -> IO (Maybe RNAFasta)
readFasta path = do
  raw :: T.Text <- TIO.readFile path
  return (parseRNA (head (parsecFasta raw)))
