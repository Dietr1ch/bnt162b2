{-# LANGUAGE OverloadedStrings #-}

module Optimization ( bestCodon
                    , optimize
                    ) where

import qualified Data.Text as T

import Bio ( RNANucleobase(..)
           , RNACodon(..)
           , RNAAminoAcid(..)
           , RNAFasta(..)
           , from
           )

-- Codon maximizing Gs and Cs. Preferring Gs over Cs.
--
-- assert (from . bestCodon == Id)
--
-- TODO(dietr1ch): Figure out a way to do fancy compile-time evaluation
-- maximizing Gs and Cs
--
bestCodon :: RNAAminoAcid -> RNACodon
-- Non-polar
-- ---------
bestCodon Phenylalanine = RNACodon U U C -- Optimal
bestCodon Leucine = RNACodon C U G
--bestCodon Leucine = RNACodon C U C
bestCodon Isoleucine = RNACodon A U C -- Optimal
bestCodon Methionine = RNACodon A U G -- Unique
--bestCodon Valine = RNACodon G U G
bestCodon Valine = RNACodon G U C

--bestCodon Proline = RNACodon C C G
bestCodon Proline = RNACodon C C C
--bestCodon Alanine = RNACodon G C G
bestCodon Alanine = RNACodon G C C

bestCodon Tryptophan = RNACodon U G G  -- Unique
--bestCodon Glycine = RNACodon G G G
bestCodon Glycine = RNACodon G G C

-- Polar
-- -----
--bestCodon Serine = RNACodon U C G
bestCodon Serine = RNACodon U C C
--bestCodon Serine = RNACodon A G C
--bestCodon Threonine = RNACodon A C G
bestCodon Threonine = RNACodon A C C

bestCodon Tyrosine = RNACodon U A C -- Optimal
bestCodon Glutamine = RNACodon C A G -- Optimal
bestCodon Asparagine = RNACodon A A C -- Optimal
bestCodon Cysteine = RNACodon U G C -- Optimal

-- Basic
-- -----
bestCodon Histidine = RNACodon C A C -- Optimal
bestCodon Lysine = RNACodon A A G -- Optimal
bestCodon Arginine = RNACodon A G G -- Optimal

-- Acidic
-- ------
bestCodon AsparticAcid = RNACodon G A C -- Optimal
bestCodon GlutamicAcid = RNACodon G A G -- Optimal

-- Stop
bestCodon Stop = RNACodon U A G
--bestCodon Stop = RNACodon U G A


optimize :: RNAFasta -> RNAFasta
optimize rna = rna { header=T.concat ["Optimized | ", (header rna)]
                   , codons=(map (bestCodon . from) (codons rna)) }
