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

  writeFile "vaccine.fasta.rnafasta" (fShow $ vaccine)
  writeFile "covid.fasta.rnafasta" (fShow $ covid)
  writeFile "optimized.fasta.rnafasta" (fShow opt)

  writeFile "opt-vaccine.fasta_diff"   (dShow (diff opt vaccine))
  writeFile "covid-opt.fasta_diff"     (dShow (diff covid opt))
  writeFile "covid-vaccine.fasta_diff" (dShow (diff covid vaccine))
