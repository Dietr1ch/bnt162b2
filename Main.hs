{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Maybe

import Bio ( RNANucleobase(..)
           , RNACodon(..)
           , RNAFasta(..)
           , from
           , fShow
           )
import Parser ( readFasta
              )
import Optimization ( bestCodon
                    , optimize
                    )

main :: IO ()
main = do
  putStrLn "Lib self-test"
  putStrLn "============="
  putStrLn "some Codon"
  let someCodon = RNACodon U U U
  let someAA = from someCodon
  putStrLn (show someCodon)
  putStrLn (show someAA)

  putStrLn ""
  putStrLn "optimized Codon"
  let best = (bestCodon someAA)
  let bestAA = from best
  putStrLn (show best)
  putStrLn (show bestAA)

  putStrLn ""
  putStrLn "Data"
  putStrLn "===="

  Just fastaVaccine <- readFasta "vaccine-s.fasta"
  Just fastaCovid   <- readFasta "ncov-s.fasta"

  putStrLn ""
  putStrLn "Vaccine:"
  putStrLn $ fShow $ fastaVaccine

  putStrLn ""
  putStrLn "Covid:"
  putStrLn $ fShow $ fastaCovid

  putStrLn ""
  putStrLn "Optimized:"
  putStrLn $ fShow $ optimize fastaCovid
