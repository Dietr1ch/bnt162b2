{-# LANGUAGE OverloadedStrings #-}

module Bio.Optimization ( bestCodon
                        , optimize
                        ) where

import qualified Data.Text as T

import Bio.RNA ( Nucleobase(..)
               , Codon(..)
               , AminoAcid(..)
               , Fasta(..)
               , from
               )

-- Codon maximizing Gs and Cs. Preferring Gs over Cs.
--
-- assert (from . bestCodon == Id)
--
-- TODO(dietr1ch): Figure out a way to do fancy compile-time evaluation
-- maximizing Gs and Cs
--
bestCodon :: AminoAcid -> Codon
-- Non-polar
-- ---------
bestCodon Phenylalanine = Codon U U C -- Optimal
bestCodon Leucine = Codon C U G
-- bestCodon Leucine = Codon C U C -- -102
bestCodon Isoleucine = Codon A U C -- Optimal
bestCodon Methionine = Codon A U G -- Unique
bestCodon Valine = Codon G U G
-- bestCodon Valine = Codon G U C  -- -79

-- bestCodon Proline = Codon C C G -- -26
bestCodon Proline = Codon C C C
-- bestCodon Alanine = Codon G C G  -- -65
bestCodon Alanine = Codon G C C

bestCodon Tryptophan = Codon U G G  -- Unique
-- bestCodon Glycine = Codon G G G  -- -63
bestCodon Glycine = Codon G G C

-- Polar
-- -----
-- bestCodon Serine = Codon U C G -- -64
-- bestCodon Serine = Codon U C C -- -42
bestCodon Serine = Codon A G C
-- bestCodon Threonine = Codon A C G -- -63
bestCodon Threonine = Codon A C C

bestCodon Tyrosine = Codon U A C -- Optimal
bestCodon Glutamine = Codon C A G -- Optimal
bestCodon Asparagine = Codon A A C -- Optimal
bestCodon Cysteine = Codon U G C -- Optimal

-- Basic
-- -----
bestCodon Histidine = Codon C A C -- Optimal
bestCodon Lysine = Codon A A G -- Optimal
bestCodon Arginine = Codon A G G -- Optimal

-- Acidic
-- ------
bestCodon AsparticAcid = Codon G A C -- Optimal
bestCodon GlutamicAcid = Codon G A G -- Optimal

-- Stop
-- bestCodon Stop = Codon U A G -- -1
bestCodon Stop = Codon U G A -- 0
-- bestCodon Stop = Codon U G A -- 0


optimize :: Fasta -> Fasta
optimize rna = rna { header=T.concat ["Optimized | ", (header rna)]
                   , codons=(map (bestCodon . from) (codons rna)) }
