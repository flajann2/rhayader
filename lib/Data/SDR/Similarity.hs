-- |
-- Module: Data.SDR.Similarity
--
-- Normalized and asymmetric similarity measures (roadmap section 1).
--
-- Raw 'Data.SDR.Algebras.overlap' can't be compared across SDRs of
-- different weights; these can. 'jaccard' and 'hammingDistance' already
-- live in "Data.SDR.Algebras"; this module adds Dice, cosine,
-- containment, the Tversky index, and the Jaccard /distance/ (which,
-- with Hamming, is a true metric and so supports triangle-inequality
-- pruning in search).
--
-- Convention for degenerate inputs: whenever a denominator is zero
-- (both SDRs empty, or the pattern being tested is empty) the
-- similarity is 0. An empty pattern should never "match" anything,
-- so this keeps threshold queries conservative.

module Data.SDR.Similarity
  ( dice
  , cosine
  , containment
  , tversky
  , jaccardDistance
  ) where

import qualified Data.IntSet as IS
import Data.SDR.Core

-- | Sørensen–Dice coefficient: 2|x ∩ y| / (w_x + w_y).
dice :: SDR -> SDR -> Double
dice a b = requireSameWidth a b $
  let s = sdrWeight a + sdrWeight b
  in if s == 0 then 0
     else 2 * fromIntegral (ovl a b) / fromIntegral s

-- | Cosine similarity of the indicator vectors: |x ∩ y| / sqrt (w_x w_y).
cosine :: SDR -> SDR -> Double
cosine a b = requireSameWidth a b $
  let wa = sdrWeight a
      wb = sdrWeight b
  in if wa == 0 || wb == 0 then 0
     else fromIntegral (ovl a b) / sqrt (fromIntegral wa * fromIntegral wb)

-- | Containment of the first SDR in the second: |x ∩ y| / w_x.
--
-- Asymmetric. @containment pattern context@ answers "how much of this
-- pattern is present in that context?" -- the HTM-style subset query.
-- Equals 1 exactly when x ⊆ y (and x is non-empty).
containment :: SDR -> SDR -> Double
containment x y = requireSameWidth x y $
  let wx = sdrWeight x
  in if wx == 0 then 0
     else fromIntegral (ovl x y) / fromIntegral wx

-- | Tversky index
--
-- > T_{α,β}(x, y) = |x ∩ y| / (|x ∩ y| + α|x \ y| + β|y \ x|)
--
-- α = β = 1 gives Jaccard, α = β = ½ gives Dice. Skewing the weights
-- gives graded subset queries: α = 1, β = 0 is 'containment'.
-- α and β must be non-negative.
tversky :: Double -> Double -> SDR -> SDR -> Double
tversky alpha beta x y
  | alpha < 0 || beta < 0 = error "SDR.tversky: alpha and beta must be non-negative"
  | otherwise = requireSameWidth x y $
      let o     = fromIntegral (ovl x y)
          onlyX = fromIntegral (sdrWeight x) - o
          onlyY = fromIntegral (sdrWeight y) - o
          den   = o + alpha * onlyX + beta * onlyY
      in if den == 0 then 0 else o / den

-- | Jaccard distance, 1 − J(x, y). A true metric on sets; two empty
-- SDRs are at distance 0 (identity of indiscernibles).
jaccardDistance :: SDR -> SDR -> Double
jaccardDistance a b = requireSameWidth a b $
  let u = IS.size (IS.union (sdrBits a) (sdrBits b))
  in if u == 0 then 0
     else 1 - fromIntegral (ovl a b) / fromIntegral u

-- internal ---------------------------------------------------------------

ovl :: SDR -> SDR -> Int
ovl a b = IS.size (IS.intersection (sdrBits a) (sdrBits b))
{-# INLINE ovl #-}
