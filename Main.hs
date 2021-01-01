module Main where

import Bio ( RNANucleobase(..)
           , RNACodon(..)
           , from)
import Optimization ( bestCodon
                    )

main :: IO ()
main = do
  let someCodon = RNACodon U U U
  let someAA = from someCodon
  putStrLn (show someCodon)
  putStrLn (show someAA)
  let best = (bestCodon someAA)
  let bestAA = from best
  putStrLn (show best)
  putStrLn (show bestAA)
