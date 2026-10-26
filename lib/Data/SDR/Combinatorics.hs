{-# LANGUAGE BangPatterns #-}

-- |
-- Module: Data.SDR.Combinatorics
--
-- Combinatorics proper (roadmap section 5): capacity, the
-- combinatorial number system (combinadics), uniform sampling, and
-- expected multi-way overlap. Also hosts the exact and log-space
-- binomial coefficients that "Data.SDR.Stats" builds on.
--
-- Combinadics give a bijection between the weight-w SDRs of width n
-- and the integers [0, C(n, w)):
--
-- > rank(x) = Σ_{i=0}^{w-1} C(c_i, i+1),   c_0 < c_1 < … < c_{w-1}
--
-- which yields perfect hashing ('rank'), compact serialization,
-- exhaustive enumeration ('allSDRs') and uniform sampling
-- ('uniformSDR' = 'unrank' of a uniform integer). Floyd's algorithm
-- ('floydSample') is the cheaper way to sample when you don't need
-- the rank.

module Data.SDR.Combinatorics
  ( -- * Binomial coefficients
    choose
  , logChoose
  , logFactorial
    -- * Capacity
  , capacity
  , capacityBits
    -- * Combinatorial number system
  , rank
  , unrank
  , allSDRs
    -- * Sampling
  , floydSample
  , uniformSDR
    -- * Multi-way coincidence
  , expectedKWayOverlap
  ) where

import qualified Data.IntSet as IS
import System.Random (RandomGen, uniformR)
import Data.SDR.Core

--------------------------------------------------------------------------------
-- Binomial coefficients
--------------------------------------------------------------------------------

-- | Exact binomial coefficient C(n, k). Zero outside 0 ≤ k ≤ n.
choose :: Integer -> Integer -> Integer
choose n k
  | k < 0 || k > n = 0
  | otherwise      = go 1 1
  where
    k' = min k (n - k)
    -- acc = C(n - k' + i - 1, i - 1) on entry; each step is exact.
    go !acc i
      | i > k'    = acc
      | otherwise = go (acc * (n - k' + i) `div` i) (i + 1)

-- | Natural log of m!. Exact summation for small m, Stirling series
-- (to the 1/m^5 term) above that; absolute error < 1e-12 everywhere.
logFactorial :: Int -> Double
logFactorial m
  | m < 0     = error "SDR.logFactorial: negative argument"
  | m < 32    = sum [ log (fromIntegral i) | i <- [2 .. m] ]
  | otherwise =
      (x + 0.5) * log x - x + 0.5 * log (2 * pi)
        + 1 / (12 * x) - 1 / (360 * x ^ (3 :: Int)) + 1 / (1260 * x ^ (5 :: Int))
  where
    x = fromIntegral m :: Double

-- | Natural log of C(n, k) via log-gamma; −∞ outside 0 ≤ k ≤ n.
-- Use this instead of 'choose' when n is large and you only need
-- probabilities.
logChoose :: Int -> Int -> Double
logChoose n k
  | k < 0 || k > n = -1 / 0
  | otherwise      = logFactorial n - logFactorial k - logFactorial (n - k)

--------------------------------------------------------------------------------
-- Capacity
--------------------------------------------------------------------------------

-- | Number of distinct SDRs of width n and weight w: C(n, w).
capacity :: Int -> Int -> Integer
capacity n w = choose (toInteger n) (toInteger w)

-- | Information carried by one SDR of width n and weight w:
-- log2 C(n, w) bits.
capacityBits :: Int -> Int -> Double
capacityBits n w = logChoose n w / log 2

--------------------------------------------------------------------------------
-- Combinadics
--------------------------------------------------------------------------------

-- | Combinadic rank of an SDR among all SDRs of its width and weight,
-- in [0, C(n, w)). A perfect hash.
rank :: SDR -> Integer
rank s = sum [ choose (toInteger c) (i + 1)
             | (i, c) <- zip [0 ..] (IS.toAscList (sdrBits s)) ]

-- | Inverse of 'rank': the SDR of width n and weight w with the given
-- rank. Errors if the rank is outside [0, C(n, w)).
--
-- > unrank (sdrWidth x) (sdrWeight x) (rank x) == x
unrank :: Int -> Int -> Integer -> SDR
unrank n w r0
  | w < 0 || w > n            = error "SDR.unrank: weight out of range"
  | r0 < 0 || r0 >= capacity n w = error "SDR.unrank: rank out of range"
  | otherwise = mkSDRUnsafe n (IS.fromList (go w n r0))
  where
    -- Greedy decode from the highest element down: c_{i-1} is the
    -- largest c < hi with C(c, i) <= r.
    go 0 _  _ = []
    go i hi r =
      let ii = toInteger i
          c  = largest ii (i - 1) (hi - 1) r
      in c : go (i - 1) c (r - choose (toInteger c) ii)

    -- Largest c in [lo, hi] with C(c, i) <= r. C(lo, i) = 0 when
    -- lo = i - 1, so the predicate holds at lo; it is monotone in c.
    largest :: Integer -> Int -> Int -> Integer -> Int
    largest ii lo hi r
      | lo >= hi  = lo
      | otherwise =
          let mid = (lo + hi + 1) `div` 2
          in if choose (toInteger mid) ii <= r
               then largest ii mid hi r
               else largest ii lo (mid - 1) r

-- | Every SDR of width n and weight w, lazily, in rank order.
-- There are 'capacity' n w of them -- only enumerate small spaces.
allSDRs :: Int -> Int -> [SDR]
allSDRs n w = map (unrank n w) [0 .. capacity n w - 1]

--------------------------------------------------------------------------------
-- Sampling
--------------------------------------------------------------------------------

-- | Uniform random SDR of width n and weight w by Floyd's algorithm:
-- exactly w draws, no rejection, no rank arithmetic.
floydSample :: RandomGen g => Int -> Int -> g -> (SDR, g)
floydSample n w g0
  | w < 0 || w > n = error "SDR.floydSample: weight out of range"
  | otherwise      = go (n - w) IS.empty g0
  where
    go j !s g
      | j >= n    = (mkSDRUnsafe n s, g)
      | otherwise =
          let (t, g') = uniformR (0, j) g
              s'      = if IS.member t s then IS.insert j s else IS.insert t s
          in go (j + 1) s' g'

-- | Uniform random SDR via 'unrank' of a uniform rank. Slower than
-- 'floydSample' but exercises the combinadic bijection, and the
-- drawn rank can be kept as a compact identifier.
uniformSDR :: RandomGen g => Int -> Int -> g -> (SDR, g)
uniformSDR n w g =
  let (r, g') = uniformR (0, capacity n w - 1) g
  in (unrank n w r, g')

--------------------------------------------------------------------------------
-- Multi-way coincidence
--------------------------------------------------------------------------------

-- | Expected size of the intersection of k independent random SDRs of
-- width n and weight w: n (w/n)^k. Each bit is active in one SDR with
-- probability w/n, so this is exact in expectation for independent
-- draws. Use it to size multi-way coincidence detectors.
expectedKWayOverlap :: Int -> Int -> Int -> Double
expectedKWayOverlap n w k
  | n <= 0    = error "SDR.expectedKWayOverlap: width must be positive"
  | k < 0     = error "SDR.expectedKWayOverlap: k must be non-negative"
  | otherwise = fromIntegral n * (fromIntegral w / fromIntegral n) ^ k
