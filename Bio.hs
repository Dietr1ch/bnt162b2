module Bio ( RNANucleobase(..)
           , RNACodon(..)
           , RNAAminoAcid(..)
           , from
           ) where

-- RNA facts

data RNANucleobase = A
                   | C
                   | G
                   | U
                   deriving (Eq, Show)

data RNACodon = RNACodon RNANucleobase RNANucleobase RNANucleobase
                deriving (Eq, Show)

data RNAAminoAcid = Phenylalanine
                  | Leucine
                  | Isoleucine
                  | Methionine
                  | Valine
                  | Proline
                  | Alanine
                  | Tryptophan
                  | Glycine
                  -- Polar
                  | Serine
                  | Threonine
                  | Tyrosine
                  | Glutamine
                  | Asparagine
                  | Cysteine
                  -- Basic
                  | Histidine
                  | Lysine
                  | Arginine
                  -- Acidic
                  | AsparticAcid
                  | GlutamicAcid
                  -- Stop
                  | Stop
                  deriving (Eq, Show)

-- https://en.wikipedia.org/wiki/DNA_and_RNA_codon_tables#Standard_RNA_codon_table
from :: RNACodon -> RNAAminoAcid
--- Codons U U *
from (RNACodon U U U) = Phenylalanine
from (RNACodon U U C) = Phenylalanine
from (RNACodon U U _) = Leucine
--- Codons C U *
from (RNACodon C U _) = Leucine
--- Codons A U *
from (RNACodon A U G) = Methionine
from (RNACodon A U _) = Isoleucine
--- Codons G U *
from (RNACodon G U _) = Valine

-- Codons * C *
from (RNACodon U C _) = Serine
from (RNACodon C C _) = Proline
from (RNACodon A C _) = Threonine
from (RNACodon G C _) = Alanine

--- Codons U A *
from (RNACodon U A U) = Tyrosine
from (RNACodon U A C) = Tyrosine
from (RNACodon U A _) = Stop  -- Ochre + Amber
--- Codons C A *
from (RNACodon C A U) = Histidine
from (RNACodon C A C) = Histidine
from (RNACodon C A _) = Glutamine
--- Codons A A *
from (RNACodon A A U) = Asparagine
from (RNACodon A A C) = Asparagine
from (RNACodon A A _) = Lysine
--- Codons G A *
from (RNACodon G A U) = AsparticAcid
from (RNACodon G A C) = AsparticAcid
from (RNACodon G A _) = GlutamicAcid

-- Codons U G *
from (RNACodon U G A) = Stop
from (RNACodon U G G) = Tryptophan
from (RNACodon U G _) = Cysteine
-- Codons C G *
from (RNACodon C G _) = Arginine
-- Codons A G *
from (RNACodon A G U) = Serine
from (RNACodon A G C) = Serine
from (RNACodon A G _) = Arginine
-- Codons G G *
from (RNACodon G G _) = Glycine
