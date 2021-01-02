{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Maybe

import Bio.Diff (diff)
import Bio.Formatting (PrettyPrint(..))
import Bio.Optimization ( bestCodon
                        , optimize
                        )
import Bio.Parser (readFasta)
import Bio.RNA ( Nucleobase(..)
               , Codon(..)
               , Fasta(..)
               , from
               )

main :: IO ()
main = do
  Just vaccine <- readFasta "vaccine-s.fasta"
  Just covid   <- readFasta "ncov-s.fasta"
  let  opt = optimize covid

  putStrLn "Writing output files"
  writeFile "out/vaccine.fasta.rnafasta" (pp vaccine)
  writeFile "out/covid.fasta.rnafasta" (pp covid)
  writeFile "out/optimized.fasta.rnafasta" (pp opt)

  putStrLn "Writing diffs"
  writeFile "out/diffs/opt-vaccine.fasta_diff"   (pp (diff opt vaccine))
  writeFile "out/diffs/covid-opt.fasta_diff"     (pp (diff covid opt))
  writeFile "out/diffs/covid-vaccine.fasta_diff" (pp (diff covid vaccine))
