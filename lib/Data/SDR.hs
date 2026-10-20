{-# LANGUAGE BangPatterns #-}

-- |
-- Module: Data.SDR
--
-- Sparse Distributed Representation core: the universal input/output
-- interface for data entering the Rhayader spiking network.
--
-- Design notes:
--   * SDR carries its width explicitly (not phantom-typed) so that
--     combinators can do a cheap runtime dimensionality check rather
--     than silently operating across mismatched spaces.
--   * Combinators (overlap, union, intersectSDR, difference, ...) are
--     total but 'error' on width mismatch -- a mismatch here is a
--     programmer bug (wrong encoder wired to wrong pooler), not a
--     recoverable runtime condition. If SDRs ever get constructed from
--     untrusted/external width parameters (e.g. deserialized genomes),
--     switch the boundary functions to Either SDRError SDR.
--   * Active-bit indices double as input-neuron IDs when bridging into
--     Spiking NEAT: if the genome's input neuron ID space equals the
--     SDR's bit-index space (0 .. width-1), an active bit *is* an
--     input neuron ID, with no translation layer required.

module Data.SDR
  ( module Data.SDR.Core
  , module Data.SDR.Pooling
  , module Data.SDR.Infix
  , module Data.SDR.Algebras
  , module Data.SDR.Encoders
  , module Data.SDR.Slicing
  , module Data.SDR.Similarity
  , module Data.SDR.Stats
  , module Data.SDR.Combinatorics
  , module Data.SDR.Composition
  , module Data.SDR.Search
  ) where

import Data.SDR.Core
import Data.SDR.Pooling
import Data.SDR.Infix
import Data.SDR.Encoders
import Data.SDR.Slicing
import Data.SDR.Algebras
import Data.SDR.Similarity
import Data.SDR.Stats
import Data.SDR.Combinatorics
import Data.SDR.Composition
import Data.SDR.Search
