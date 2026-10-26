-- |
-- Module: Data.SDR.Search
--
-- Search at scale (roadmap section 8).
--
--   * __MinHash__: with a random permutation π,
--     Pr[min π(x) = min π(y)] = J(x, y). A signature of k such minima
--     estimates Jaccard similarity from k integers.
--   * __Banded MinHash (LSH)__: split the signature into b bands of r
--     rows; two SDRs become candidates if any band matches exactly.
--     The candidate probability 1 − (1 − J^r)^b is an S-curve, giving
--     sublinear nearest-neighbour lookup over millions of SDRs.
--   * __Inverted index__: bit → SDR ids. The exact-match complement:
--     exact overlap counts against everything stored that shares a bit.
--
-- The MinHash permutations are approximated with the 2-universal
-- family h(x) = (a·x + b) mod p, p = 2³¹ − 1. This is the standard
-- practical choice; it is not exactly min-wise independent, so
-- estimates carry a small bias in addition to the 1/√k sampling error.
-- Bit indices must be below p.
--
-- Both indexes here are pure, in-memory, and built on containers --
-- the reference implementation. A terabyte-scale store would keep the
-- same interfaces over mutable or memory-mapped tables.

module Data.SDR.Search
  ( -- * MinHash
    MinHasher
  , mkMinHasher
  , minHasherSize
  , Signature
  , minHash
  , estimateJaccard
    -- * Banded MinHash (LSH)
  , LSHIndex
  , emptyLSH
  , lshInsert
  , lshDelete
  , lshCandidates
  , lshQuery
  , lshSize
  , lshCandidateProbability
    -- * Inverted index
  , InvertedIndex
  , emptyInverted
  , invInsert
  , invDelete
  , overlapCounts
  , invQuery
  , invExact
  ) where

import qualified Data.IntSet as IS
import Data.IntSet (IntSet)
import qualified Data.IntMap.Strict as IM
import Data.IntMap.Strict (IntMap)
import qualified Data.Map.Strict as M
import Data.List (sortBy, foldl')
import Data.Ord (comparing, Down(..))
import Data.Word (Word64)
import System.Random (RandomGen, uniformR)
import Data.SDR.Core
import Data.SDR.Algebras (jaccard)

--------------------------------------------------------------------------------
-- MinHash
--------------------------------------------------------------------------------

-- | k hash functions (a_i, b_i) standing in for random permutations.
newtype MinHasher = MinHasher [(Word64, Word64)]
  deriving (Eq, Show)

-- | A MinHash signature: one minimum per hash function.
type Signature = [Word64]

mersenne31 :: Word64
mersenne31 = 2147483647

-- | Draw k hash functions.
mkMinHasher :: RandomGen g => Int -> g -> (MinHasher, g)
mkMinHasher k g0 = go k [] g0
  where
    go 0 acc g = (MinHasher (reverse acc), g)
    go i acc g =
      let (a, g')  = uniformR (1, mersenne31 - 1) g
          (b, g'') = uniformR (0, mersenne31 - 1) g'
      in go (i - 1) ((a, b) : acc) g''

minHasherSize :: MinHasher -> Int
minHasherSize (MinHasher hs) = length hs

-- | Signature of an SDR. The empty SDR hashes to all-'maxBound', so
-- two empty SDRs share a signature (estimate 1) even though 'jaccard'
-- defines their similarity as 0; don't index empty SDRs.
minHash :: MinHasher -> SDR -> Signature
minHash (MinHasher hs) s =
  [ IS.foldl' (\m i -> min m ((a * fromIntegral i + b) `mod` mersenne31)) maxBound bits
  | (a, b) <- hs ]
  where
    bits = sdrBits s

-- | Fraction of agreeing signature positions: an unbiased-ish estimate
-- of J(x, y) with standard error ≈ sqrt (J(1−J)/k).
estimateJaccard :: Signature -> Signature -> Double
estimateJaccard s1 s2
  | k == 0    = 0
  | otherwise = fromIntegral (length (filter id (zipWith (==) s1 s2))) / fromIntegral k
  where
    k = min (length s1) (length s2)

--------------------------------------------------------------------------------
-- Banded MinHash
--------------------------------------------------------------------------------

-- | LSH index over SDRs, keyed by caller-supplied integer ids.
data LSHIndex = LSHIndex
  { lshHasher :: !MinHasher
  , lshBands  :: !Int
  , lshRows   :: !Int
  , lshTables :: !(M.Map (Int, [Word64]) IntSet)
  , lshStore  :: !(IntMap SDR)
  }

-- | Empty index with b bands of r rows. The hasher must have exactly
-- b · r functions. Pick r and b so the S-curve threshold
-- (1/b)^(1/r) sits at the Jaccard similarity you care about.
emptyLSH :: MinHasher -> Int -> Int -> LSHIndex
emptyLSH h b r
  | b <= 0 || r <= 0           = error "SDR.emptyLSH: bands and rows must be positive"
  | minHasherSize h /= b * r   = error "SDR.emptyLSH: hasher size must equal bands * rows"
  | otherwise                  = LSHIndex h b r M.empty IM.empty

bandKeys :: LSHIndex -> SDR -> [(Int, [Word64])]
bandKeys ix s = zip [0 .. lshBands ix - 1] (chunks (lshRows ix) (minHash (lshHasher ix) s))
  where
    chunks _ [] = []
    chunks r xs = let (h, t) = splitAt r xs in h : chunks r t

-- | Insert (or replace) an SDR under an id.
lshInsert :: Int -> SDR -> LSHIndex -> LSHIndex
lshInsert sid s ix0 =
  ix { lshTables = foldl' (\t k -> M.insertWith IS.union k (IS.singleton sid) t)
                          (lshTables ix) (bandKeys ix s)
     , lshStore  = IM.insert sid s (lshStore ix) }
  where
    ix = lshDelete sid ix0

-- | Remove an id (no-op if absent).
lshDelete :: Int -> LSHIndex -> LSHIndex
lshDelete sid ix = case IM.lookup sid (lshStore ix) of
  Nothing -> ix
  Just s  -> ix { lshTables = foldl' (flip (M.update drop1)) (lshTables ix) (bandKeys ix s)
                , lshStore  = IM.delete sid (lshStore ix) }
  where
    drop1 ids = let ids' = IS.delete sid ids
                in if IS.null ids' then Nothing else Just ids'

-- | Ids sharing at least one band with the query.
lshCandidates :: SDR -> LSHIndex -> IntSet
lshCandidates q ix = IS.unions [ M.findWithDefault IS.empty k (lshTables ix) | k <- bandKeys ix q ]

-- | Top-k candidates by exact Jaccard similarity (candidates from LSH,
-- then reranked exactly), best first.
lshQuery :: Int -> SDR -> LSHIndex -> [(Int, Double)]
lshQuery k q ix =
  take k . sortBy (comparing (Down . snd)) $
    [ (sid, jaccard q s)
    | sid <- IS.toList (lshCandidates q ix)
    , Just s <- [IM.lookup sid (lshStore ix)] ]

lshSize :: LSHIndex -> Int
lshSize = IM.size . lshStore

-- | Probability two SDRs with Jaccard similarity J become candidates
-- under b bands of r rows: 1 − (1 − J^r)^b.
lshCandidateProbability :: Int -> Int -> Double -> Double
lshCandidateProbability b r j = 1 - (1 - j ^ r) ^ b

--------------------------------------------------------------------------------
-- Inverted index
--------------------------------------------------------------------------------

-- | Bit → ids postings, plus each stored SDR (for exact matching and
-- deletion).
data InvertedIndex = InvertedIndex
  { invPostings :: !(IntMap IntSet)
  , invStore    :: !(IntMap SDR)
  }

emptyInverted :: InvertedIndex
emptyInverted = InvertedIndex IM.empty IM.empty

-- | Insert (or replace) an SDR under an id.
invInsert :: Int -> SDR -> InvertedIndex -> InvertedIndex
invInsert sid s ix0 = InvertedIndex
  { invPostings = IS.foldl' (\m i -> IM.insertWith IS.union i (IS.singleton sid) m)
                            (invPostings ix) (sdrBits s)
  , invStore    = IM.insert sid s (invStore ix) }
  where
    ix = invDelete sid ix0

-- | Remove an id (no-op if absent).
invDelete :: Int -> InvertedIndex -> InvertedIndex
invDelete sid ix = case IM.lookup sid (invStore ix) of
  Nothing -> ix
  Just s  -> InvertedIndex
    { invPostings = IS.foldl' (flip (IM.update drop1)) (invPostings ix) (sdrBits s)
    , invStore    = IM.delete sid (invStore ix) }
  where
    drop1 ids = let ids' = IS.delete sid ids
                in if IS.null ids' then Nothing else Just ids'

-- | Exact overlap of the query with every stored SDR that shares at
-- least one bit. Cost is proportional to the postings touched.
overlapCounts :: SDR -> InvertedIndex -> IntMap Int
overlapCounts q ix = IM.unionsWith (+)
  [ IM.fromSet (const 1) ids
  | i <- IS.toList (sdrBits q)
  , Just ids <- [IM.lookup i (invPostings ix)] ]

-- | Ids whose overlap with the query is at least θ, with the overlap,
-- best first. Pair with 'Data.SDR.Stats.thresholdFor' to pick θ from a
-- false-positive rate. θ must be at least 1 (θ = 0 would match every
-- stored SDR, including ones sharing no bits).
invQuery :: Int -> SDR -> InvertedIndex -> [(Int, Int)]
invQuery theta q ix
  | theta < 1 = error "SDR.invQuery: threshold must be at least 1"
  | otherwise = sortBy (comparing (Down . snd))
                  [ p | p@(_, o) <- IM.toList (overlapCounts q ix), o >= theta ]

-- | Ids of stored SDRs exactly equal to the query (an empty query
-- matches nothing, since it touches no postings).
invExact :: SDR -> InvertedIndex -> [Int]
invExact q ix =
  [ sid | (sid, o) <- IM.toList (overlapCounts q ix)
        , o == sdrWeight q
        , Just s <- [IM.lookup sid (invStore ix)]
        , s == q ]
