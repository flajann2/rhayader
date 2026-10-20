-- |
-- Module: Data.SDR.Laws
--
-- The algebraic laws of Data.SDR as plain 'Bool' predicates, so the
-- library carries its own specification without depending on a test
-- framework. The QuickCheck suite (test/SDRSpec.hs) feeds these random
-- inputs; you can also call them from your own properties or from
-- GHCi.
--
-- Statistical laws from the roadmap -- J(x ⊗ y, x) ≈ 0 and
-- J(⟨x ∨ y⟩, x) > J_random -- hold on average rather than for every
-- input, so they live in the test suite as averaged checks.

module Data.SDR.Laws
  ( -- * Similarity (section 1)
    lawTverskyIsJaccard
  , lawTverskyIsDice
  , lawHammingFromOverlap
  , lawJaccardTriangle
  , lawHammingTriangle
  , lawContainmentSubset
    -- * Combinatorics (section 5)
  , lawRankRoundTrip
  , lawRankInRange
    -- * Composition (section 6)
  , lawPermuteWeight
  , lawPermuteJaccard
  , lawPermuteRoundTrip
  , lawBindWeight
  , lawBindInverse
  , lawBindCommutative
  , lawBindIdentity
  , lawThinSubset
    -- * Encoder contracts (section 7)
  , lawEncoderContract
  , lawEncoderMonotone
    -- * Set algebra (existing)
  , lawConcatSplit
  ) where

import qualified Data.IntSet as IS
import Data.SDR.Core
import Data.SDR.Algebras
import Data.SDR.Slicing (splitSDR)
import Data.SDR.Similarity
import Data.SDR.Combinatorics
import Data.SDR.Composition
import Data.SDR.Encoders

approxEq :: Double -> Double -> Bool
approxEq a b = abs (a - b) <= 1e-12 * max 1 (max (abs a) (abs b))

-- Similarity ---------------------------------------------------------------

-- | T_{1,1} = J
lawTverskyIsJaccard :: SDR -> SDR -> Bool
lawTverskyIsJaccard x y = approxEq (tversky 1 1 x y) (jaccard x y)

-- | T_{½,½} = D
lawTverskyIsDice :: SDR -> SDR -> Bool
lawTverskyIsDice x y = approxEq (tversky 0.5 0.5 x y) (dice x y)

-- | H(x, y) = w_x + w_y − 2|x ∩ y|
lawHammingFromOverlap :: SDR -> SDR -> Bool
lawHammingFromOverlap x y =
  hammingDistance x y == sdrWeight x + sdrWeight y - 2 * overlap x y

-- | 1 − J is a metric: d(x, z) ≤ d(x, y) + d(y, z)
lawJaccardTriangle :: SDR -> SDR -> SDR -> Bool
lawJaccardTriangle x y z =
  jaccardDistance x z <= jaccardDistance x y + jaccardDistance y z + 1e-12

lawHammingTriangle :: SDR -> SDR -> SDR -> Bool
lawHammingTriangle x y z =
  hammingDistance x z <= hammingDistance x y + hammingDistance y z

-- | A non-empty x is fully contained in x ∪ y.
lawContainmentSubset :: SDR -> SDR -> Bool
lawContainmentSubset x y =
  sdrWeight x == 0 || containment x (union x y) == 1

-- Combinatorics -----------------------------------------------------------

-- | unrank ∘ rank = id
lawRankRoundTrip :: SDR -> Bool
lawRankRoundTrip x = unrank (sdrWidth x) (sdrWeight x) (rank x) == x

-- | 0 ≤ rank x < C(n, w)
lawRankInRange :: SDR -> Bool
lawRankInRange x = r >= 0 && r < capacity (sdrWidth x) (sdrWeight x)
  where r = rank x

-- Composition -------------------------------------------------------------

lawPermuteWeight :: Permutation -> SDR -> Bool
lawPermuteWeight p x = sdrWeight (permute p x) == sdrWeight x

-- | J(ρx, ρy) = J(x, y)
lawPermuteJaccard :: Permutation -> SDR -> SDR -> Bool
lawPermuteJaccard p x y = jaccard (permute p x) (permute p y) == jaccard x y

lawPermuteRoundTrip :: Permutation -> SDR -> Bool
lawPermuteRoundTrip p x = unpermute p (permute p x) == x

-- | |x ⊗ y| = w
lawBindWeight :: BlockSpec -> SDR -> SDR -> Bool
lawBindWeight spec x y = sdrWeight (bind spec x y) == bsBlocks spec

-- | (x ⊗ y) ⊘ y = x
lawBindInverse :: BlockSpec -> SDR -> SDR -> Bool
lawBindInverse spec x y = unbind spec (bind spec x y) y == x

lawBindCommutative :: BlockSpec -> SDR -> SDR -> Bool
lawBindCommutative spec x y = bind spec x y == bind spec y x

lawBindIdentity :: BlockSpec -> SDR -> Bool
lawBindIdentity spec x = bind spec x (identityBlockCode spec) == x

-- | Thinning only ever removes bits: ⟨z⟩ ⊆ z.
lawThinSubset :: Thinner -> Int -> SDR -> Bool
lawThinSubset t target z = sdrBits (thinTo t target z) `IS.isSubsetOf` sdrBits z

-- Encoder contracts -------------------------------------------------------

-- | Overlap stays inside the encoder's stated bounds.
lawEncoderContract :: MetricEncoder e => e -> Domain e -> Domain e -> Bool
lawEncoderContract e a b = lo <= o && o <= hi
  where
    (lo, hi) = overlapBounds e (domainDistance e a b)
    o        = overlap (encodeWith e a) (encodeWith e b)

-- | Overlap is non-increasing in distance, up to one resolution step.
lawEncoderMonotone :: MetricEncoder e => e -> Domain e -> Domain e -> Domain e -> Bool
lawEncoderMonotone e a b c
  | domainDistance e a c >= domainDistance e a b + encoderResolution e =
      overlap (encodeWith e a) (encodeWith e c) <= overlap (encodeWith e a) (encodeWith e b)
  | otherwise = True

-- Set algebra -------------------------------------------------------------

-- | Splitting undoes concatenation.
lawConcatSplit :: SDR -> SDR -> Bool
lawConcatSplit a b = splitSDR (sdrWidth a) (concatSDR a b) == (a, b)
