-- |
-- Module: Data.SDR.Stats
--
-- Statistics for SDRs (roadmap sections 2, 3, 4 and 9):
--
--   * Overlap significance (Ahmad & Hawkins 2015): the hypergeometric
--     tail turns every match threshold θ from a magic number into a
--     chosen false-positive rate ('thresholdFor').
--   * Union capacity: how many patterns a union (temporal pooling)
--     holds before a fresh SDR falsely matches it ('unionCapacity').
--   * Noise / subsampling robustness: probability a true match still
--     clears θ ('survivalProbability'). With the overlap tail this
--     gives the full ROC curve ('rocCurve').
--   * Homeostasis: per-bit activation frequency, population entropy,
--     a boost rule that targets it, and mutual information between
--     SDR streams.
--
-- Probabilities come in two flavours: an exact 'Rational' version
-- (Integer binomials -- use for checking and small n) and a 'Double'
-- version summed in log space via log-gamma (use in hot paths and at
-- large n).

module Data.SDR.Stats
  ( -- * Hypergeometric tail
    hypergeomTailExact
  , hypergeomTail
    -- * Overlap significance
  , overlapTailExact
  , overlapTail
  , overlapPValue
  , expectedOverlap
  , overlapVariance
  , overlapZScore
  , thresholdFor
    -- * Union capacity
  , unionDensity
  , unionFalsePositive
  , unionCapacity
    -- * Noise and subsampling robustness
  , survivalProbability
  , survivalProbabilityNoise
  , rocCurve
    -- * Homeostasis
  , bitFrequencies
  , binaryEntropy
  , populationEntropy
  , maxPopulationEntropy
  , entropyEfficiency
  , homeostaticBoost
    -- * Mutual information
  , mutualInformation
  , bitMutualInformation
    -- * Utilities
  , logSumExp
  ) where

import qualified Data.IntSet as IS
import qualified Data.IntMap.Strict as IM
import Data.IntMap.Strict (IntMap)
import qualified Data.Map.Strict as M
import Numeric (log1p, expm1)
import Data.SDR.Core
import Data.SDR.Combinatorics (choose, logChoose)

--------------------------------------------------------------------------------
-- Generic hypergeometric tail
--------------------------------------------------------------------------------

-- | Exact upper tail of the hypergeometric distribution. Drawing d
-- items without replacement from a population of size N containing
-- K "successes", the probability of at least θ successes:
--
-- > Σ_{b=θ}^{min(K,d)} C(K,b) C(N−K, d−b) / C(N, d)
hypergeomTailExact :: Int  -- ^ population size N
                   -> Int  -- ^ successes in population K
                   -> Int  -- ^ draws d
                   -> Int  -- ^ threshold θ
                   -> Rational
hypergeomTailExact nPop k d theta =
  checkHyper "hypergeomTailExact" nPop k d $
    fromInteger (sum [ choose kI (toInteger b) * choose (nI - kI) (dI - toInteger b)
                     | b <- [lo .. hi] ])
      / fromInteger (choose nI dI)
  where
    (lo, hi) = hyperRange nPop k d theta
    nI = toInteger nPop; kI = toInteger k; dI = toInteger d

-- | 'hypergeomTailExact' in 'Double', summed in log space. Accurate
-- to ~1e-12 relative and fast at large N.
hypergeomTail :: Int -> Int -> Int -> Int -> Double
hypergeomTail nPop k d theta =
  checkHyper "hypergeomTail" nPop k d $
    if lo > hi then 0
    else clamp01 . exp $
           logSumExp [ logChoose k b + logChoose (nPop - k) (d - b) | b <- [lo .. hi] ]
             - logChoose nPop d
  where
    (lo, hi) = hyperRange nPop k d theta

hyperRange :: Int -> Int -> Int -> Int -> (Int, Int)
hyperRange nPop k d theta = (maximum [theta, 0, d - (nPop - k)], min k d)

checkHyper :: String -> Int -> Int -> Int -> a -> a
checkHyper fn nPop k d x
  | nPop < 0             = error ("SDR." ++ fn ++ ": negative population")
  | k < 0 || k > nPop    = error ("SDR." ++ fn ++ ": successes out of range")
  | d < 0 || d > nPop    = error ("SDR." ++ fn ++ ": draws out of range")
  | otherwise            = x

--------------------------------------------------------------------------------
-- Overlap significance (roadmap section 2)
--------------------------------------------------------------------------------

-- | Exact probability that a uniformly random SDR of weight w (width n)
-- overlaps a fixed SDR of weight w_x in at least θ bits:
--
-- > P(|x ∩ y| ≥ θ) = Σ_{b=θ}^{min(w_x,w)} C(w_x,b) C(n−w_x, w−b) / C(n, w)
overlapTailExact :: Int  -- ^ width n
                 -> Int  -- ^ weight of the fixed SDR, w_x
                 -> Int  -- ^ weight of the random SDR, w
                 -> Int  -- ^ threshold θ
                 -> Rational
overlapTailExact = hypergeomTailExact

-- | 'overlapTailExact' in 'Double' (log-space). This is the false
-- positive rate of the match rule "overlap ≥ θ".
overlapTail :: Int -> Int -> Int -> Int -> Double
overlapTail = hypergeomTail

-- | p-value of the observed overlap between two concrete SDRs: the
-- chance a random SDR of y's weight would overlap x at least as much.
overlapPValue :: SDR -> SDR -> Double
overlapPValue x y = requireSameWidth x y $
  overlapTail (sdrWidth x) (sdrWeight x) (sdrWeight y)
              (IS.size (IS.intersection (sdrBits x) (sdrBits y)))

-- | Expected overlap with a random SDR: E = w_x w / n.
expectedOverlap :: Int -> Int -> Int -> Double
expectedOverlap n wx w
  | n <= 0    = error "SDR.expectedOverlap: width must be positive"
  | otherwise = fromIntegral wx * fromIntegral w / fromIntegral n

-- | Variance of the overlap with a random SDR:
--
-- > Var = w_x w (n − w_x)(n − w) / (n² (n − 1))
overlapVariance :: Int -> Int -> Int -> Double
overlapVariance n wx w
  | n <= 0    = error "SDR.overlapVariance: width must be positive"
  | n == 1    = 0
  | otherwise = (wx' * w' * (n' - wx') * (n' - w')) / (n' * n' * (n' - 1))
  where
    n' = fromIntegral n; wx' = fromIntegral wx; w' = fromIntegral w

-- | z-score of an observed overlap against the random-SDR null.
-- Returns 0 when the variance is 0 (the overlap is then forced).
overlapZScore :: Int -> Int -> Int -> Int -> Double
overlapZScore n wx w o
  | v <= 0    = 0
  | otherwise = (fromIntegral o - expectedOverlap n wx w) / sqrt v
  where
    v = overlapVariance n wx w

-- | Smallest threshold θ whose false-positive rate is at most ε:
-- the principled replacement for a hand-tuned match threshold.
-- Always exists: at θ = min w_x w + 1 the rate is 0.
thresholdFor :: Int     -- ^ width n
             -> Int     -- ^ stored pattern weight w_x
             -> Int     -- ^ query weight w
             -> Double  -- ^ acceptable false-positive rate ε
             -> Int
thresholdFor n wx w eps =
  head [ t | t <- [0 .. min wx w + 1], overlapTail n wx w t <= eps ]

--------------------------------------------------------------------------------
-- Union capacity (roadmap section 3)
--------------------------------------------------------------------------------

-- | Expected density of a union of M random SDRs of weight w:
--
-- > p_M = 1 − (1 − w/n)^M
--
-- Exact in expectation for independent uniform patterns.
unionDensity :: Int -> Int -> Int -> Double
unionDensity n w m
  | n <= 0    = error "SDR.unionDensity: width must be positive"
  | m <= 0    = 0
  | w >= n    = 1
  | otherwise = negate (expm1' (fromIntegral m * log1p' (negate (fromIntegral w / fromIntegral n))))

-- | Approximate probability that a fresh SDR of weight w falsely
-- matches (overlap ≥ θ) a union of M stored patterns. Treats the
-- union's bits as independent with density p_M -- a binomial tail:
--
-- > P_fp(M) ≈ Σ_{b=θ}^{w} C(w,b) p_M^b (1 − p_M)^{w−b}
unionFalsePositive :: Int  -- ^ width n
                   -> Int  -- ^ weight w
                   -> Int  -- ^ threshold θ
                   -> Int  -- ^ number of stored patterns M
                   -> Double
unionFalsePositive n w theta m = binomialTail w (unionDensity n w m) theta

-- | How many patterns a union holds before it lies to you: the
-- largest M with 'unionFalsePositive' n w θ M ≤ ε.
--
-- Degenerate cases: if θ > w the union can never falsely match, and if
-- ε ≥ 1 any rate is acceptable -- both return 'maxBound'. If θ ≤ 0
-- every query matches, so the capacity is 0.
unionCapacity :: Int     -- ^ width n
              -> Int     -- ^ weight w
              -> Int     -- ^ threshold θ
              -> Double  -- ^ acceptable false-positive rate ε
              -> Int
unionCapacity n w theta eps
  | theta > w || eps >= 1 = maxBound
  | theta <= 0            = 0
  | otherwise             = grow 1
  where
    ok m = unionFalsePositive n w theta m <= eps
    limit = 2 ^ (40 :: Int) :: Int
    -- ok 0 holds (empty union, θ > 0). Double until it fails.
    grow hi
      | hi > limit = maxBound
      | ok hi      = grow (hi * 2)
      | otherwise  = search (hi `div` 2) hi
    -- invariant: ok lo, not (ok hi)
    search lo hi
      | hi - lo <= 1 = lo
      | otherwise    = let mid = (lo + hi) `div` 2
                       in if ok mid then search mid hi else search lo mid

--------------------------------------------------------------------------------
-- Noise and subsampling robustness (roadmap section 4)
--------------------------------------------------------------------------------

-- | Probability that a true match still clears θ when k of the
-- pattern's w bits are lost (corrupted) and only s of the w bits are
-- stored/sampled:
--
-- > P(survive) = Σ_{b=θ}^{s} C(w−k, b) C(k, s−b) / C(w, s)
survivalProbability :: Int  -- ^ pattern weight w
                    -> Int  -- ^ bits lost k
                    -> Int  -- ^ bits stored s
                    -> Int  -- ^ threshold θ
                    -> Double
survivalProbability w k s theta
  | k < 0 || k > w = error "SDR.survivalProbability: k out of range"
  | otherwise      = hypergeomTail w (w - k) s theta

-- | 'survivalProbability' with a corruption fraction ρ: k = round (ρ w).
survivalProbabilityNoise :: Int -> Double -> Int -> Int -> Double
survivalProbabilityNoise w rho s theta
  | rho < 0 || rho > 1 = error "SDR.survivalProbabilityNoise: rho must be in [0, 1]"
  | otherwise          = survivalProbability w (round (rho * fromIntegral w)) s theta

-- | Full ROC curve for a design (n, w, s, ρ): for every threshold θ in
-- [0, s], the pair (false-positive rate, true-positive rate).
--
-- FPR: a stored subsample of s bits matched by a random weight-w SDR.
-- TPR: the stored subsample matched by the true pattern after a
-- fraction ρ of its bits has been corrupted.
rocCurve :: Int -> Int -> Int -> Double -> [(Int, Double, Double)]
rocCurve n w s rho =
  [ (t, overlapTail n s w t, survivalProbabilityNoise w rho s t) | t <- [0 .. s] ]

--------------------------------------------------------------------------------
-- Homeostasis (roadmap section 9)
--------------------------------------------------------------------------------

-- | Per-bit activation frequency f_i over a stream of SDRs. Bits never
-- active are absent (f_i = 0). Empty input gives an empty map.
bitFrequencies :: [SDR] -> IntMap Double
bitFrequencies [] = IM.empty
bitFrequencies sdrs = IM.map (\c -> fromIntegral c / total) counts
  where
    total  = fromIntegral (length sdrs) :: Double
    counts = IM.fromListWith (+) [ (i, 1 :: Int) | s <- sdrs, i <- IS.toList (sdrBits s) ]

-- | Binary entropy in bits: H_b(f) = −f log2 f − (1 − f) log2 (1 − f).
binaryEntropy :: Double -> Double
binaryEntropy f
  | f <= 0 || f >= 1 = 0
  | otherwise        = negate (f * logBase 2 f + (1 - f) * logBase 2 (1 - f))

-- | Population entropy H_pop = Σ_i H_b(f_i), from 'bitFrequencies'.
-- Absent bits have f_i = 0 and contribute nothing.
populationEntropy :: IntMap Double -> Double
populationEntropy = IM.foldl' (\acc f -> acc + binaryEntropy f) 0

-- | Upper bound on 'populationEntropy' for width n and mean weight w:
-- n H_b(w/n), reached when every bit fires at the same rate w/n. This
-- is the measurable target boosting should drive towards.
maxPopulationEntropy :: Int -> Int -> Double
maxPopulationEntropy n w
  | n <= 0    = error "SDR.maxPopulationEntropy: width must be positive"
  | otherwise = fromIntegral n * binaryEntropy (fromIntegral w / fromIntegral n)

-- | H_pop / (n H_b(w/n)) in [0, 1]. 1 means perfectly even usage of
-- all bits; low values mean a few bits are doing all the work.
entropyEfficiency :: Int -> Int -> IntMap Double -> Double
entropyEfficiency n w freqs
  | hmax <= 0 = 0
  | otherwise = populationEntropy freqs / hmax
  where
    hmax = maxPopulationEntropy n w

-- | HTM-style exponential boost factors, one per bit in [0, n):
--
-- > b_i = exp (β (target − f_i))
--
-- Under-active bits get b_i > 1, over-active ones b_i < 1. The map is
-- total over the width so it composes with
-- 'Data.SDR.Pooling.applyBoost' (which intersects keys).
homeostaticBoost :: Double        -- ^ strength β ≥ 0
                 -> Double        -- ^ target density, usually w / n
                 -> Int           -- ^ width n
                 -> IntMap Double -- ^ bit frequencies f_i
                 -> IntMap Double
homeostaticBoost beta target n freqs =
  IM.fromDistinctAscList
    [ (i, exp (beta * (target - IM.findWithDefault 0 i freqs))) | i <- [0 .. n - 1] ]

--------------------------------------------------------------------------------
-- Mutual information
--------------------------------------------------------------------------------

-- | Plug-in estimate of the mutual information, in bits, between two
-- aligned SDR streams, treating each whole SDR as a symbol:
--
-- > I(X;Y) = Σ p(x,y) log2 (p(x,y) / (p(x) p(y)))
--
-- Shows whether an encoder or pooler actually carries signal about its
-- input. The plug-in estimator is biased upward when few samples are
-- spread over many distinct symbols; compare against a shuffled
-- baseline before trusting small values.
mutualInformation :: [(SDR, SDR)] -> Double
mutualInformation [] = 0
mutualInformation pairs = sum
  [ pxy * logBase 2 (pxy / (px * py))
  | ((x, y), cxy) <- M.toList joint
  , let pxy = fromIntegral cxy / total
        px  = fromIntegral (marginalX M.! x) / total
        py  = fromIntegral (marginalY M.! y) / total
  ]
  where
    total     = fromIntegral (length pairs) :: Double
    joint     = M.fromListWith (+) [ (p, 1 :: Int) | p <- pairs ]
    marginalX = M.fromListWith (+) [ (x, 1 :: Int) | (x, _) <- pairs ]
    marginalY = M.fromListWith (+) [ (y, 1 :: Int) | (_, y) <- pairs ]

-- | Mutual information, in bits, between bit i of stream X and bit j
-- of stream Y. Much better conditioned than 'mutualInformation' for
-- short streams: a 2×2 contingency table.
bitMutualInformation :: Int -> Int -> [(SDR, SDR)] -> Double
bitMutualInformation _ _ [] = 0
bitMutualInformation i j pairs = sum
  [ term (cell a b) (rowX a) (colY b) | a <- [False, True], b <- [False, True] ]
  where
    total = fromIntegral (length pairs) :: Double
    obs   = [ (IS.member i (sdrBits x), IS.member j (sdrBits y)) | (x, y) <- pairs ]
    cell a b = fromIntegral (length (filter (== (a, b)) obs)) / total
    rowX a   = fromIntegral (length (filter ((== a) . fst) obs)) / total
    colY b   = fromIntegral (length (filter ((== b) . snd) obs)) / total
    term pxy px py
      | pxy <= 0  = 0
      | otherwise = pxy * logBase 2 (pxy / (px * py))

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- | Numerically stable log (Σ exp x_i). −∞ for an empty list.
logSumExp :: [Double] -> Double
logSumExp [] = -1 / 0
logSumExp xs
  | isInfinite m && m < 0 = m
  | otherwise             = m + log (sum [ exp (x - m) | x <- xs ])
  where
    m = maximum xs

-- | Upper tail of Binomial(w, p): P(X ≥ θ).
binomialTail :: Int -> Double -> Int -> Double
binomialTail w p theta
  | theta <= 0 = 1
  | theta > w  = 0
  | p <= 0     = 0
  | p >= 1     = 1
  | otherwise  = clamp01 . exp $ logSumExp
      [ logChoose w b + fromIntegral b * log p + fromIntegral (w - b) * log1p' (negate p)
      | b <- [theta .. w] ]

clamp01 :: Double -> Double
clamp01 = max 0 . min 1

-- log1p / expm1 with explicit Double types (Floating's methods).
log1p' :: Double -> Double
log1p' = log1p

expm1' :: Double -> Double
expm1' = expm1
