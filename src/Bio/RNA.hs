{-# LANGUAGE FlexibleInstances #-}

module Bio.RNA ( Nucleobase(..)
               , Codon(..)
               , AminoAcid(..)
               , from
               , Fasta(..)
               ) where

import Bio.Formatting (PrettyPrint(..))

import Data.Text (Text, unpack)
import Data.List (intercalate)

data Nucleobase = A
                | C
                | G
                | U
                deriving (Eq, Show, Read)

data Codon = Codon Nucleobase Nucleobase Nucleobase
             deriving (Eq, Show, Read)

instance PrettyPrint Codon where
  pp (Codon a b c) = (show a) ++ (show b) ++ (show c)

instance PrettyPrint [Codon] where
  pp c = intercalate "\n" (map pp c)

data AminoAcid = Phenylalanine
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
               deriving (Eq, Show, Read)

-- https://en.wikipedia.org/wiki/DNA_and_RNA_codon_tables#Standard_RNA_codon_table
from :: Codon -> AminoAcid
--- Codons U U *
from (Codon U U U) = Phenylalanine
from (Codon U U C) = Phenylalanine
from (Codon U U _) = Leucine
--- Codons C U *
from (Codon C U _) = Leucine
--- Codons A U *
from (Codon A U G) = Methionine
from (Codon A U _) = Isoleucine
--- Codons G U *
from (Codon G U _) = Valine

-- Codons * C *
from (Codon U C _) = Serine
from (Codon C C _) = Proline
from (Codon A C _) = Threonine
from (Codon G C _) = Alanine

--- Codons U A *
from (Codon U A U) = Tyrosine
from (Codon U A C) = Tyrosine
from (Codon U A _) = Stop  -- Ochre + Amber
--- Codons C A *
from (Codon C A U) = Histidine
from (Codon C A C) = Histidine
from (Codon C A _) = Glutamine
--- Codons A A *
from (Codon A A U) = Asparagine
from (Codon A A C) = Asparagine
from (Codon A A _) = Lysine
--- Codons G A *
from (Codon G A U) = AsparticAcid
from (Codon G A C) = AsparticAcid
from (Codon G A _) = GlutamicAcid

-- Codons U G *
from (Codon U G A) = Stop
from (Codon U G G) = Tryptophan
from (Codon U G _) = Cysteine
-- Codons C G *
from (Codon C G _) = Arginine
-- Codons A G *
from (Codon A G U) = Serine
from (Codon A G C) = Serine
from (Codon A G _) = Arginine
-- Codons G G *
from (Codon G G _) = Glycine


data Fasta = Fasta { header :: Text
                   , codons :: [Codon]
                   } deriving (Eq, Show, Read)

instance PrettyPrint Fasta where
  pp Fasta{header=h, codons=c} = (unpack h) ++ "\n\n" ++ pp c
