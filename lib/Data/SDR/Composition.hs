{-# LANGUAGE BangPatterns #-}

-- |
-- Module: Data.SDR.Composition
--
-- Weight-preserving composition operators (roadmap section 6).
--
-- The set algebra in "Data.SDR.Algebras" breaks density: unions get
-- denser, intersections sparser. A closed algebra needs operators that
-- keep weight fixed:
--
--   * __Permutation__ ρ ('permute'): preserves weight and pairwise
--     similarity, and makes ρ(x) dissimilar to x. Sequences encode as
--     ρ²(a) ∪ ρ(b) ∪ c ('encodeSequence').
--   * __Context-dependent thinning__ (Rachkovskij & Kussul,
--     'thinTo'): brings a union back to a target weight while the
--     result stays similar to every input -- superposition with stable
--     sparsity ('superpose').
--   * __Sparse block-code binding__ (Laiho et al. 2015; Frady et al.
--     2021, 'bind' / 'unbind'): n is split into w blocks of size B with
--     exactly one active bit per block; binding adds offsets per block
--     mod B. Exactly invertible -- the key–value binding plain set
--     algebra can't do.
--
-- A note on cyclic shifts: 'shiftSDR' is cheap and is a valid ρ, but
-- for SDRs with contiguous structure (e.g. scalar encoders) a shift by
-- 1 leaves ρ(x) almost identical to x. Use a random 'Permutation' when
-- you need ρ(x) ⟂ x.

module Data.SDR.Composition
  ( -- * Permutations
    Permutation
  , permWidth
  , identityPerm
  , cyclicPerm
  , randomPermutation
  , fromImageList
  , composePerm
  , inversePerm
  , permPower
  , permute
  , unpermute
  , shiftSDR
  , encodeSequence
    -- * Context-dependent thinning
  , Thinner
  , mkThinner
  , thinnerWidth
  , thinK
  , thinTo
  , superpose
    -- * Sparse block codes
  , BlockSpec(..)
  , blockSpecWidth
  , isBlockCode
  , blockOffsets
  , fromBlockOffsets
  , identityBlockCode
  , randomBlockCode
  , bind
  , unbind
  , inverseBlockCode
  ) where

import qualified Data.IntSet as IS
import qualified Data.IntMap.Strict as IM
import Data.IntMap.Strict (IntMap)
import Data.List (foldl')
import System.Random (RandomGen, uniformR)
import Data.SDR.Core

--------------------------------------------------------------------------------
-- Permutations
--------------------------------------------------------------------------------

-- | A fixed permutation of the bit indices [0, n). Stores both
-- directions so 'unpermute' is as cheap as 'permute'.
data Permutation = Permutation
  { permWidth :: !Int
  , permFwd   :: !(IntMap Int)
  , permInv   :: !(IntMap Int)
  } deriving (Eq, Show)

mkPerm :: Int -> IntMap Int -> Permutation
mkPerm n fwd = Permutation n fwd (IM.fromList [ (v, k) | (k, v) <- IM.toList fwd ])

identityPerm :: Int -> Permutation
identityPerm n = mkPerm n (IM.fromDistinctAscList [ (i, i) | i <- [0 .. n - 1] ])

-- | Cyclic shift by k as an explicit permutation (i ↦ (i + k) mod n).
cyclicPerm :: Int -> Int -> Permutation
cyclicPerm n k
  | n <= 0    = error "SDR.cyclicPerm: width must be positive"
  | otherwise = mkPerm n (IM.fromDistinctAscList [ (i, (i + k) `mod` n) | i <- [0 .. n - 1] ])

-- | Uniform random permutation of [0, n) by Fisher–Yates.
randomPermutation :: RandomGen g => Int -> g -> (Permutation, g)
randomPermutation n g0 = go (n - 1) (IM.fromDistinctAscList [ (i, i) | i <- [0 .. n - 1] ]) g0
  where
    go i !m g
      | i <= 0    = (mkPerm n m, g)
      | otherwise =
          let (j, g') = uniformR (0, i) g
              vi = m IM.! i
              vj = m IM.! j
          in go (i - 1) (IM.insert i vj (IM.insert j vi m)) g'

-- | Build a permutation from the list of images [ρ(0), ρ(1), …].
-- Errors unless the list is a permutation of [0, n).
fromImageList :: [Int] -> Permutation
fromImageList imgs
  | IS.fromList imgs /= IS.fromDistinctAscList [0 .. n - 1] =
      error "SDR.fromImageList: not a permutation of [0, n)"
  | otherwise = mkPerm n (IM.fromDistinctAscList (zip [0 ..] imgs))
  where
    n = length imgs

-- | @composePerm p q@ applies q first, then p (function composition).
composePerm :: Permutation -> Permutation -> Permutation
composePerm p q
  | permWidth p /= permWidth q = error "SDR.composePerm: width mismatch"
  | otherwise = mkPerm (permWidth p) (IM.map (permFwd p IM.!) (permFwd q))

inversePerm :: Permutation -> Permutation
inversePerm (Permutation n f i) = Permutation n i f

-- | ρ^k for any integer k (negative powers use the inverse), by
-- repeated squaring.
permPower :: Int -> Permutation -> Permutation
permPower k p
  | k < 0     = permPower (negate k) (inversePerm p)
  | k == 0    = identityPerm (permWidth p)
  | even k    = let h = permPower (k `div` 2) p in composePerm h h
  | otherwise = composePerm p (permPower (k - 1) p)

-- | Apply ρ to every active bit. Preserves weight, and preserves every
-- similarity measure between pairs: J(ρx, ρy) = J(x, y).
permute :: Permutation -> SDR -> SDR
permute p s
  | permWidth p /= sdrWidth s = error "SDR.permute: width mismatch"
  | otherwise = mkSDRUnsafe (sdrWidth s) (IS.map (permFwd p IM.!) (sdrBits s))

-- | Apply ρ⁻¹. @unpermute p (permute p x) == x@.
unpermute :: Permutation -> SDR -> SDR
unpermute = permute . inversePerm

-- | Cyclic shift by k bits (negative k shifts the other way). The
-- cheapest ρ; see the module note on contiguous encoders.
shiftSDR :: Int -> SDR -> SDR
shiftSDR k s = mkSDRUnsafe n (IS.map (\i -> (i + k) `mod` n) (sdrBits s))
  where
    n = sdrWidth s

-- | Encode an ordered sequence by nested permutation and union:
--
-- > encodeSequence ρ [a, b, c] == ρ²(a) ∪ ρ(b) ∪ c
--
-- Earlier elements are pushed further through ρ, so order matters.
-- The result is denser than its inputs; follow with 'thinTo' to get
-- back to the target weight.
encodeSequence :: Permutation -> [SDR] -> SDR
encodeSequence p = foldl' step (emptySDR (permWidth p))
  where
    step acc x
      | sdrWidth x /= permWidth p = error "SDR.encodeSequence: width mismatch"
      | otherwise = mkSDRUnsafe (sdrWidth x)
                      (IS.union (sdrBits (permute p acc)) (sdrBits x))

--------------------------------------------------------------------------------
-- Context-dependent thinning
--------------------------------------------------------------------------------

-- | A fixed family of permutations ρ_1 … ρ_K for context-dependent
-- thinning. It must be the same family every time you thin, or the
-- similarity-preserving property is lost -- build it once from a seed.
data Thinner = Thinner
  { thinnerWidth :: !Int
  , thinnerPerms :: [Permutation]
  }

instance Show Thinner where
  show t = "Thinner<w=" ++ show (thinnerWidth t) ++ ", K=" ++ show (length (thinnerPerms t)) ++ ">"

-- | Build a thinner of width n with up to kMax random permutations.
mkThinner :: RandomGen g => Int -> Int -> g -> (Thinner, g)
mkThinner n kMax g0 = go kMax [] g0
  where
    go 0 acc g = (Thinner n (reverse acc), g)
    go k acc g = let (p, g') = randomPermutation n g in go (k - 1) (p : acc) g'

-- | Additive CDT with a fixed K:
--
-- > ⟨z⟩_K = z ∩ ⋃_{k=1}^{K} ρ_k(z)
--
-- The result is a subset of z, and grows with K.
thinK :: Thinner -> Int -> SDR -> SDR
thinK t k z
  | thinnerWidth t /= sdrWidth z = error "SDR.thinK: width mismatch"
  | otherwise = mkSDRUnsafe (sdrWidth z) (IS.intersection zb cover)
  where
    zb    = sdrBits z
    cover = IS.unions [ sdrBits (permute p z) | p <- take k (thinnerPerms t) ]

-- | Thin z to approximately the target weight: K is chosen as the
-- smallest value whose result reaches the target (it may overshoot
-- by the bits the last ρ_k contributes). Inputs already at or below
-- the target are returned unchanged. If the family runs out first,
-- the best available result is returned -- size kMax generously
-- (roughly the number of patterns you superpose, times a few).
thinTo :: Thinner -> Int -> SDR -> SDR
thinTo t target z
  | thinnerWidth t /= sdrWidth z = error "SDR.thinTo: width mismatch"
  | sdrWeight z <= target        = z
  | otherwise                    = go IS.empty (thinnerPerms t)
  where
    zb = sdrBits z
    n  = sdrWidth z
    go cover [] = mkSDRUnsafe n (IS.intersection zb cover)
    go cover (p : ps) =
      let cover' = IS.union cover (sdrBits (permute p z))
          r      = IS.intersection zb cover'
      in if IS.size r >= target then mkSDRUnsafe n r else go cover' ps

-- | Superposition with stable sparsity: thin the union of the inputs
-- back to weight w. The result stays similar to each input.
superpose :: Thinner -> Int -> [SDR] -> SDR
superpose _ _ [] = error "SDR.superpose: empty list"
superpose t w xs = thinTo t w (mkSDRUnsafe n (IS.unions (map checked xs)))
  where
    n = thinnerWidth t
    checked x
      | sdrWidth x /= n = error "SDR.superpose: width mismatch"
      | otherwise       = sdrBits x

--------------------------------------------------------------------------------
-- Sparse block codes
--------------------------------------------------------------------------------

-- | Layout of a sparse block code: 'bsBlocks' blocks (= the weight w),
-- each 'bsBlockSize' bits wide (= B). Width is w · B.
data BlockSpec = BlockSpec
  { bsBlocks    :: !Int
  , bsBlockSize :: !Int
  } deriving (Eq, Show)

blockSpecWidth :: BlockSpec -> Int
blockSpecWidth (BlockSpec w b) = w * b

-- | Does the SDR have exactly one active bit in each block?
isBlockCode :: BlockSpec -> SDR -> Bool
isBlockCode spec s = maybe False (const True) (blockOffsetsM spec s)

-- | The active offset within each block, block 0 first. Errors if the
-- SDR is not a valid block code for the spec.
blockOffsets :: BlockSpec -> SDR -> [Int]
blockOffsets spec s = case blockOffsetsM spec s of
  Just offs -> offs
  Nothing   -> error "SDR.blockOffsets: not a block code for this spec"

blockOffsetsM :: BlockSpec -> SDR -> Maybe [Int]
blockOffsetsM spec@(BlockSpec w b) s
  | sdrWidth s /= blockSpecWidth spec = Nothing
  | length idx /= w                   = Nothing
  | all (\o -> o >= 0 && o < b) offs  = Just offs
  | otherwise                         = Nothing
  where
    idx  = IS.toAscList (sdrBits s)
    -- With exactly w bits in ascending order, the code is valid iff
    -- the j-th smallest bit lies in block j.
    offs = zipWith (\j i -> i - j * b) [0 ..] idx

-- | Build a block code from per-block offsets.
fromBlockOffsets :: BlockSpec -> [Int] -> SDR
fromBlockOffsets spec@(BlockSpec w b) offs
  | length offs /= w                   = error "SDR.fromBlockOffsets: wrong number of blocks"
  | any (\o -> o < 0 || o >= b) offs   = error "SDR.fromBlockOffsets: offset out of range"
  | otherwise = mkSDRUnsafe (blockSpecWidth spec)
                  (IS.fromDistinctAscList (zipWith (\j o -> j * b + o) [0 ..] offs))

-- | The identity for 'bind': offset 0 in every block.
identityBlockCode :: BlockSpec -> SDR
identityBlockCode spec = fromBlockOffsets spec (replicate (bsBlocks spec) 0)

-- | Uniform random block code.
randomBlockCode :: RandomGen g => BlockSpec -> g -> (SDR, g)
randomBlockCode spec@(BlockSpec w b) g0 = go w [] g0
  where
    go 0 acc g = (fromBlockOffsets spec (reverse acc), g)
    go k acc g = let (o, g') = uniformR (0, b - 1) g in go (k - 1) (o : acc) g'

-- | Block-code binding:
--
-- > (x ⊗ y)_j = (x_j + y_j) mod B
--
-- Commutative, associative, weight-preserving, and the result is
-- dissimilar to both inputs.
bind :: BlockSpec -> SDR -> SDR -> SDR
bind spec x y = fromBlockOffsets spec
  (zipWith (\a c -> (a + c) `mod` bsBlockSize spec) (blockOffsets spec x) (blockOffsets spec y))

-- | Exact inverse of 'bind': @unbind spec (bind spec x y) y == x@.
--
-- > (z ⊘ y)_j = (z_j − y_j) mod B
unbind :: BlockSpec -> SDR -> SDR -> SDR
unbind spec z y = fromBlockOffsets spec
  (zipWith (\a c -> (a - c) `mod` bsBlockSize spec) (blockOffsets spec z) (blockOffsets spec y))

-- | The y⁻¹ with @bind spec y (inverseBlockCode spec y) == identityBlockCode spec@,
-- so @unbind spec z y == bind spec z (inverseBlockCode spec y)@.
inverseBlockCode :: BlockSpec -> SDR -> SDR
inverseBlockCode spec y = fromBlockOffsets spec
  (map (\a -> negate a `mod` bsBlockSize spec) (blockOffsets spec y))
