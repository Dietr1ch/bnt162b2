module Bio.Formatting (PrettyPrint(..)
                      ) where

class PrettyPrint a where
  pp :: a -> String
