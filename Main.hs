{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Maybe

import Bio ( RNANucleobase(..)
           , RNACodon(..)
           , RNAFasta(..)
           , from
           , fShow
           )
import Diff ( diff
            , dShow
            )
import Parser ( readFasta
              )
import Optimization ( bestCodon
                    , optimize
                    )

main :: IO ()
main = do
  Just vaccine <- readFasta "vaccine-s.fasta"
  Just covid   <- readFasta "ncov-s.fasta"
  let  opt = optimize covid

  writeFile "out/vaccine.fasta.rnafasta" (fShow $ vaccine)
  writeFile "out/covid.fasta.rnafasta" (fShow $ covid)
  writeFile "out/optimized.fasta.rnafasta" (fShow opt)

  writeFile "out/diffs/opt-vaccine.fasta_diff"   (dShow (diff opt vaccine))
  writeFile "out/diffs/covid-opt.fasta_diff"     (dShow (diff covid opt))
  writeFile "out/diffs/covid-vaccine.fasta_diff" (dShow (diff covid vaccine))
