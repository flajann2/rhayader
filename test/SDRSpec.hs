-- | QuickCheck laws and numerical checks for Data.SDR
-- (Similarity, Statistics & Combinatorics roadmap).
module Main (main) where

import Control.Monad (unless, forM)
import qualified Data.IntSet as IS
import qualified Data.IntMap.Strict as IM
import System.Exit (exitFailure)
import System.IO (hSetEncoding, stdout, utf8)
import System.Random (StdGen, mkStdGen)
import Test.QuickCheck

import Data.SDR hiding (choose)
import qualified Data.SDR.Combinatorics as C
import Data.SDR.Laws

--------------------------------------------------------------------------------
-- Generators
--------------------------------------------------------------------------------

seeded :: (StdGen -> (a, StdGen)) -> Gen a
seeded f = fst . f . mkStdGen <$> arbitrary

genSDR :: Int -> Int -> Gen SDR
genSDR n w = seeded (floydSample n w)

-- | Any weight, including 0 and n.
genSDRAnyWeight :: Int -> Gen SDR
genSDRAnyWeight n = do
  w <- chooseInt (0, n)
  genSDR n w

-- | Biased towards overlap: y is built from x by swapping some bits,
-- so similarity measures see the whole range, not just ~0.
genPair :: Int -> Gen (SDR, SDR)
genPair n = do
  x <- genSDRAnyWeight n
  y <- oneof [ genSDRAnyWeight n, perturb n x ]
  pure (x, y)

perturb :: Int -> SDR -> Gen SDR
perturb n x = do
  r    <- genSDRAnyWeight n
  keep <- sublistOf (IS.toList (sdrBits x))
  pure (mkSDR n (IS.union (IS.fromList keep) (IS.filter even (sdrBits r))))

data Triple = Triple SDR SDR SDR deriving Show

genTriple :: Int -> Gen Triple
genTriple n = do
  (x, y) <- genPair n
  z <- oneof [ genSDRAnyWeight n, perturb n y ]
  pure (Triple x y z)

genPerm :: Int -> Gen Permutation
genPerm n = seeded (randomPermutation n)

genSpec :: Gen BlockSpec
genSpec = BlockSpec <$> chooseInt (1, 40) <*> chooseInt (1, 64)

genBlock :: BlockSpec -> Gen SDR
genBlock spec = seeded (randomBlockCode spec)

width :: Int
width = 256

--------------------------------------------------------------------------------
-- Properties
--------------------------------------------------------------------------------

props :: [(String, Property)]
props =
  [ -- Section 1
    ("Tversky(1,1) == Jaccard",    forAll (genPair width) (uncurry lawTverskyIsJaccard))
  , ("Tversky(1/2,1/2) == Dice",   forAll (genPair width) (uncurry lawTverskyIsDice))
  , ("Hamming == wx + wy - 2o",    forAll (genPair width) (uncurry lawHammingFromOverlap))
  , ("1 - J triangle inequality",  forAll (genTriple 64) (\(Triple x y z) -> lawJaccardTriangle x y z))
  , ("Hamming triangle inequality",forAll (genTriple 64) (\(Triple x y z) -> lawHammingTriangle x y z))
  , ("containment x (x ∪ y) == 1", forAll (genPair width) (uncurry lawContainmentSubset))
  , ("similarities in [0,1], symmetric", forAll (genPair width) $ \(x, y) ->
        let ms = [dice, cosine, jaccard]
        in all (\m -> m x y >= 0 && m x y <= 1 + 1e-12 && m x y == m y x) ms)
  , ("cosine >= Dice >= Jaccard",  forAll (genPair width) $ \(x, y) ->
        cosine x y + 1e-12 >= dice x y && dice x y + 1e-12 >= jaccard x y)

    -- Section 5
  , ("unrank . rank == id",        forAll (chooseInt (1, 80) >>= genSDRAnyWeight) lawRankRoundTrip)
  , ("rank in [0, C(n,w))",        forAll (chooseInt (1, 80) >>= genSDRAnyWeight) lawRankInRange)
  , ("rank is a bijection (n=10,w=4)", once $
        let xs = allSDRs 10 4
        in map rank xs == [0 .. capacity 10 4 - 1]
           && IS.size (IS.fromList (map (fromInteger . rank) xs)) == 210)
  , ("uniformSDR has the right weight", forAll (chooseInt (1, 200)) $ \n ->
        forAll (chooseInt (0, n)) $ \w ->
          forAll (seeded (uniformSDR n w)) $ \s -> sdrWeight s == w && sdrWidth s == n)
  , ("choose matches Pascal",      forAll (chooseInt (1, 60)) $ \n -> forAll (chooseInt (1, n)) $ \k ->
        let n' = toInteger n; k' = toInteger k
        in C.choose n' k' == C.choose (n' - 1) (k' - 1) + C.choose (n' - 1) k')
  , ("logChoose ~ log . choose",   forAll (chooseInt (0, 3000)) $ \n -> forAll (chooseInt (0, n)) $ \k ->
        let exact = log (fromInteger (C.choose (toInteger n) (toInteger k)) :: Double)
        in isInfinite exact || abs (logChoose n k - exact) <= 1e-9 * max 1 exact)

    -- Section 2
  , ("overlapTail ~ overlapTailExact", forAll (chooseInt (1, 300)) $ \n ->
        forAll (chooseInt (0, n)) $ \wx -> forAll (chooseInt (0, n)) $ \w ->
          forAll (chooseInt (0, min wx w + 1)) $ \t ->
            let e = fromRational (overlapTailExact n wx w t) :: Double
            in abs (overlapTail n wx w t - e) <= 1e-9 * max 1e-300 e + 1e-300)
  , ("tail at θ=0 is 1, past max is 0", forAll (chooseInt (1, 500)) $ \n ->
        forAll (chooseInt (0, n)) $ \wx -> forAll (chooseInt (0, n)) $ \w ->
          overlapTailExact n wx w 0 == 1 && overlapTailExact n wx w (min wx w + 1) == 0)
  , ("thresholdFor meets ε, minimal", forAll (chooseInt (64, 4096)) $ \n ->
        forAll (chooseInt (1, 64)) $ \w -> forAll (elements [1e-3, 1e-6, 1e-9, 1e-12]) $ \eps ->
          let t = thresholdFor n w w eps
          in overlapTail n w w t <= eps && (t == 0 || overlapTail n w w (t - 1) > eps))

    -- Section 3
  , ("unionCapacity is the boundary", forAll (chooseInt (512, 4096)) $ \n ->
        forAll (chooseInt (10, 50)) $ \w -> forAll (chooseInt (w `div` 2, w)) $ \t ->
          forAll (elements [1e-2, 1e-4, 1e-6]) $ \eps ->
            let m = unionCapacity n w t eps
            in m == maxBound
               || (unionFalsePositive n w t m <= eps && unionFalsePositive n w t (m + 1) > eps))
  , ("unionCapacity monotone in ε", forAll (chooseInt (10, 50)) $ \w ->
        unionCapacity 2048 w (w `div` 2) 1e-6 <= unionCapacity 2048 w (w `div` 2) 1e-3)

    -- Section 4
  , ("survival: no loss, store all, θ<=w => 1", forAll (chooseInt (1, 100)) $ \w ->
        forAll (chooseInt (0, w)) $ \t -> abs (survivalProbability w 0 w t - 1) < 1e-12)
  , ("survival decreasing in loss", forAll (chooseInt (2, 100)) $ \w ->
        forAll (chooseInt (0, w - 1)) $ \k -> forAll (chooseInt (1, w)) $ \s ->
          forAll (chooseInt (0, s)) $ \t ->
            survivalProbability w (k + 1) s t <= survivalProbability w k s t + 1e-12)

    -- Section 6
  , ("permute preserves weight",   forAll (genPerm width) $ \p -> forAll (genSDRAnyWeight width) (lawPermuteWeight p))
  , ("J(ρx, ρy) == J(x, y)",       forAll (genPerm width) $ \p -> forAll (genPair width) (uncurry (lawPermuteJaccard p)))
  , ("unpermute . permute == id",  forAll (genPerm width) $ \p -> forAll (genSDRAnyWeight width) (lawPermuteRoundTrip p))
  , ("permPower k == k-fold compose", forAll (genPerm 50) $ \p -> forAll (chooseInt (-6, 6)) $ \k ->
        forAll (genSDRAnyWeight 50) $ \x ->
          let step = if k >= 0 then permute p else unpermute p
          in permute (permPower k p) x == iterate step x !! abs k)
  , ("cyclicPerm == shiftSDR",     forAll (chooseInt (-300, 300)) $ \k -> forAll (genSDRAnyWeight width) $ \x ->
        permute (cyclicPerm width k) x == shiftSDR k x)
  , ("encodeSequence [a,b,c] == ρ²a ∪ ρb ∪ c", forAll (genPerm width) $ \p ->
        forAll ((,,) <$> genSDR width 8 <*> genSDR width 8 <*> genSDR width 8) $ \(a, b, c) ->
          encodeSequence p [a, b, c] == union (permPower 2 p `permute` a) (permute p b) `union` c)
  , ("|x ⊗ y| == w",               forAll genSpec $ \s -> forAll (genBlock s) $ \x -> forAll (genBlock s) (lawBindWeight s x))
  , ("(x ⊗ y) ⊘ y == x",           forAll genSpec $ \s -> forAll (genBlock s) $ \x -> forAll (genBlock s) (lawBindInverse s x))
  , ("bind commutative",           forAll genSpec $ \s -> forAll (genBlock s) $ \x -> forAll (genBlock s) (lawBindCommutative s x))
  , ("bind identity",              forAll genSpec $ \s -> forAll (genBlock s) (lawBindIdentity s))
  , ("bind y⁻¹ == unbind y",       forAll genSpec $ \s -> forAll (genBlock s) $ \z -> forAll (genBlock s) $ \y ->
        bind s z (inverseBlockCode s y) == unbind s z y)
  , ("isBlockCode rejects non-codes", forAll genSpec $ \s -> bsBlockSize s > 1 ==>
        forAll (genSDR (blockSpecWidth s) (bsBlocks s)) $ \x ->
          isBlockCode s x == all (\j -> IS.size (IS.filter (\i -> i `div` bsBlockSize s == j) (sdrBits x)) == 1)
                                 [0 .. bsBlocks s - 1])
  , ("thinning ⊆ input",           forAll (seeded (mkThinner width 16)) $ \t ->
        forAll (genSDRAnyWeight width) (lawThinSubset t 10))

    -- Section 7
  , ("scalar encoder contract",    forAll genEncoder $ \e -> forAll (genVal e) $ \a -> forAll (genVal e) $ \b ->
        lawEncoderContract e a b)
  , ("scalar encoder monotone",    forAll genEncoder $ \e -> forAll (genVal e) $ \a -> forAll (genVal e) $ \b ->
        forAll (genVal e) $ \c -> lawEncoderMonotone e a b c)
  , ("scalar encoder weight",      forAll genEncoder $ \e -> forAll (genVal e) $ \a ->
        sdrWeight (encodeWith e a) == seActive e)

    -- Section 8
  , ("inverted index overlap is exact", forAll (vectorOf 30 (genSDR 128 10)) $ \xs -> forAll (genSDR 128 10) $ \q ->
        let ix = foldr (uncurry invInsert) emptyInverted (zip [0 ..] xs)
            got = overlapCounts q ix
        in and [ IM.findWithDefault 0 i got == overlap q x | (i, x) <- zip [0 ..] xs ])
  , ("invExact finds stored SDRs", forAll (vectorOf 20 (genSDR 128 10)) $ \xs ->
        let ix = foldr (uncurry invInsert) emptyInverted (zip [0 ..] xs)
        in and [ i `elem` invExact x ix | (i, x) <- zip [0 ..] xs ])
  , ("invDelete removes",          forAll (vectorOf 10 (genSDR 128 10)) $ \xs ->
        let ix = foldr (uncurry invInsert) emptyInverted (zip [0 ..] xs)
            ix' = invDelete 0 ix
        in 0 `notElem` invExact (head xs) ix' && IM.notMember 0 (overlapCounts (head xs) ix'))
  , ("LSH finds an exact duplicate", forAll (seeded (mkMinHasher 64)) $ \h ->
        forAll (vectorOf 30 (genSDR 1024 20)) $ \xs ->
          let ix = foldr (uncurry lshInsert) (emptyLSH h 16 4) (zip [0 ..] xs)
          in and [ case lshQuery 1 x ix of { ((_, j) : _) -> j == 1; [] -> False } | x <- xs ])
  , ("lshDelete removes",          forAll (seeded (mkMinHasher 64)) $ \h ->
        forAll (vectorOf 10 (genSDR 1024 20)) $ \xs ->
          let ix = lshDelete 3 (foldr (uncurry lshInsert) (emptyLSH h 16 4) (zip [0 ..] xs))
          in lshSize ix == 9 && 3 `IS.notMember` lshCandidates (xs !! 3) ix)

    -- Section 9
  , ("binary entropy symmetric, max 1", forAll (choose (0, 1)) $ \f ->
        abs (binaryEntropy f - binaryEntropy (1 - f)) < 1e-12 && binaryEntropy f <= 1 + 1e-12)
  , ("population entropy <= max",  forAll (vectorOf 50 (genSDR 128 8)) $ \xs ->
        populationEntropy (bitFrequencies xs) <= maxPopulationEntropy 128 8 + 1e-9)
  , ("MI(X;X) == H(X), MI >= 0",   forAll (listOf1 (elements (allSDRs 6 2))) $ \xs ->
        forAll (vectorOf (length xs) (elements (allSDRs 6 2))) $ \ys ->
          let hx = mutualInformation (zip xs xs)
              counts = IM.elems (IM.fromListWith (+) [ (fromInteger (rank x), 1 :: Int) | x <- xs ])
              tot = fromIntegral (length xs) :: Double
              entropy = negate (sum [ p * logBase 2 p | c <- counts, let p = fromIntegral c / tot ])
          in abs (hx - entropy) < 1e-9 && mutualInformation (zip xs ys) >= -1e-9)
  ]
  where
    genEncoder = do
      n  <- chooseInt (20, 400)
      w  <- chooseInt (1, n - 1)
      lo <- choose (-1000, 1000)
      sp <- choose (1e-3, 1e4)
      pure (mkScalarEncoder n w lo (lo + sp))
    -- Values mostly in range, sometimes outside (clamping).
    genVal e = let lo = seMin e; hi = seMax e; sp = hi - lo
               in frequency [ (9, choose (lo, hi)), (1, choose (lo - sp, hi + sp)) ]

--------------------------------------------------------------------------------
-- Averaged / numerical checks (laws that hold in expectation)
--------------------------------------------------------------------------------

check :: String -> Bool -> String -> IO Bool
check name ok detail = do
  putStrLn ((if ok then "+++ OK, " else "*** FAILED: ") ++ name ++ "  [" ++ detail ++ "]")
  pure ok

samples :: Int -> (StdGen -> (a, StdGen)) -> Int -> [a]
samples k f seed = take k (go (mkStdGen seed))
  where go g = let (a, g') = f g in a : go g'

statistical :: IO [Bool]
statistical = do
  let spec = BlockSpec 40 64
      xs   = samples 500 (randomBlockCode spec) 1
      ys   = samples 500 (randomBlockCode spec) 2
      jBound = mean [ jaccard (bind spec x y) x | (x, y) <- zip xs ys ]
  c1 <- check "J(x ⊗ y, x) ≈ 0" (jBound < 0.02) ("mean J = " ++ show jBound)

  let n = 2048; w = 40
      (thinner, _) = mkThinner n 64 (mkStdGen 7)
      as = samples 300 (floydSample n w) 3
      bs = samples 300 (floydSample n w) 4
      thinned = [ superpose thinner w [a, b] | (a, b) <- zip as bs ]
      jThin = mean [ jaccard t a | (t, a) <- zip thinned as ]
      jRand = mean [ jaccard a b | (a, b) <- zip as bs ]
      wThin = mean [ fromIntegral (sdrWeight t) | t <- thinned ] :: Double
  c2 <- check "J(⟨x ∨ y⟩, x) > J_random" (jThin > 10 * jRand && jThin > 0.2)
              ("J_thin = " ++ show jThin ++ ", J_random = " ++ show jRand)
  c3 <- check "thinned weight ≈ w" (abs (wThin - fromIntegral w) <= 3)
              ("mean weight = " ++ show wThin ++ ", target " ++ show w)

  -- CDT must be deterministic & similarity-preserving: thinning a+b and
  -- a+b' (b' close to b) gives similar results.
  let jStable = mean [ jaccard (superpose thinner w [a, b]) (superpose thinner w [a, b'])
                     | (a, b) <- zip as bs
                     , let b' = mkSDR n (IS.insert ((IS.findMin (sdrBits b) + 1) `mod` n) (IS.deleteMin (sdrBits b))) ]
  c4 <- check "CDT similarity-preserving" (jStable > 0.7) ("mean J = " ++ show jStable)

  -- Monte Carlo check of the analytic union density.
  let m = 30
      unions = [ mkSDR n (IS.unions (map sdrBits (samples m (floydSample n w) s))) | s <- [100 .. 160] ]
      dEmp = mean [ sparsity u | u <- unions ]
      dAna = unionDensity n w m
  c5 <- check "union density matches 1-(1-w/n)^M" (abs (dEmp - dAna) < 0.01)
              ("empirical " ++ show dEmp ++ ", analytic " ++ show dAna)

  -- Monte Carlo check of overlap mean/variance.
  let fixed = head (samples 1 (floydSample n w) 9)
      os = [ fromIntegral (overlap fixed y) | y <- samples 20000 (floydSample n w) 10 ] :: [Double]
      mu = mean os
      var = mean [ (o - mu) ^ (2 :: Int) | o <- os ]
  c6 <- check "overlap mean/variance match E and Var"
              (abs (mu - expectedOverlap n w w) < 0.03 && abs (var - overlapVariance n w w) < 0.05)
              ("mean " ++ show mu ++ " vs " ++ show (expectedOverlap n w w)
               ++ ", var " ++ show var ++ " vs " ++ show (overlapVariance n w w))

  -- Ahmad & Hawkins headline: n=2048, w=40, θ=20 false-positive rate is tiny.
  let fp = overlapTail 2048 40 40 20
  c7 <- check "A&H: P(overlap ≥ 20 | n=2048,w=40) < 1e-20" (fp < 1e-20) (show fp)

  -- MinHash estimate tracks true Jaccard.
  let (h, _) = mkMinHasher 256 (mkStdGen 11)
      pairs = [ (a, mkSDR n (IS.union (IS.fromList (take 25 (IS.toList (sdrBits a)))) (sdrBits b)))
              | (a, b) <- take 100 (zip as bs) ]
      errs = [ abs (estimateJaccard (minHash h a) (minHash h b) - jaccard a b) | (a, b) <- pairs ]
  c8 <- check "MinHash estimates Jaccard" (mean errs < 0.04) ("mean abs error " ++ show (mean errs))

  -- MI: an encoder output carries information about its input; noise doesn't.
  let enc = mkScalarEncoder 100 10 0 1
      vals = [ fromIntegral (i `mod` 10) / 10 | i <- [0 .. 999 :: Int] ]
      noise = samples 1000 (floydSample 100 10) 12
      miSig = mutualInformation [ (encodeWith enc v, encodeWith enc v) | v <- vals ]
      miBit = bitMutualInformation 0 0 [ (encodeWith enc v, encodeWith enc v) | v <- vals ]
      miNoiseBit = bitMutualInformation 0 0 (zip (map (encodeWith enc) vals) noise)
  c9 <- check "MI separates signal from noise"
              (abs (miSig - logBase 2 10) < 1e-9 && miBit > 0.4 && miNoiseBit < 0.02)
              ("I(sig) = " ++ show miSig ++ ", bit-MI sig " ++ show miBit ++ ", noise " ++ show miNoiseBit)

  -- Homeostatic boost: over-active bits damped, under-active lifted.
  let bst = homeostaticBoost 2 0.1 4 (IM.fromList [(0, 0.5), (1, 0.1)])
  c10 <- check "homeostatic boost direction"
               (bst IM.! 0 < 1 && abs (bst IM.! 1 - 1) < 1e-12 && bst IM.! 2 > 1 && IM.size bst == 4)
               (show (IM.toList bst))

  pure [c1, c2, c3, c4, c5, c6, c7, c8, c9, c10]
  where
    mean xs = sum xs / fromIntegral (length xs)

--------------------------------------------------------------------------------

main :: IO ()
main = do
  hSetEncoding stdout utf8
  rs <- forM props $ \(name, p) -> do
    putStr (name ++ ": ")
    r <- quickCheckWithResult stdArgs { maxSuccess = 300 } p
    pure (isSuccess r)
  ss <- statistical
  let failed = length (filter not (rs ++ ss))
  putStrLn ""
  putStrLn (show (length rs + length ss - failed) ++ "/" ++ show (length rs + length ss) ++ " checks passed")
  unless (failed == 0) exitFailure
