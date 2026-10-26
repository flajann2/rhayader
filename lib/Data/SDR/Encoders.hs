{-# LANGUAGE TypeFamilies #-}

module Data.SDR.Encoders where

import qualified Data.IntSet as IS
import Data.SDR.Core

--------------------------------------------------------------------------------
-- Encoders
--------------------------------------------------------------------------------

data EncoderParams = EncoderParams
  { epWidth  :: !Int  -- ^ total output width, n
  , epActive :: !Int  -- ^ number of active bits per encoding, w
  } deriving (Eq, Show)

class Encodable a where
  encode :: EncoderParams -> a -> SDR

-- | Scalar encoder: a contiguous window of 'epActive' bits sliding
-- across 'epWidth' buckets by value. Nearby values overlap heavily;
-- distant values share no bits. First real instance targets the
-- crypto price feed proof-of-concept.
instance Encodable Double where
  encode (EncoderParams w active) x =
    let bucket = clampedBucketIndex w active x
    in mkSDR w (IS.fromList [bucket .. bucket + active - 1])

-- | Map a value into [0, w - active] via caller-supplied min/max/
-- resolution logic. Placeholder: wire up real domain bounds
-- (e.g. price min/max with a resolution parameter) before use --
-- this stub just clamps a raw value into range assuming x is already
-- normalized to [0, 1].
clampedBucketIndex :: Int -> Int -> Double -> Int
clampedBucketIndex w active x =
  let maxBucket = w - active
      raw       = round (x * fromIntegral maxBucket)
  in max 0 (min maxBucket raw)

-- | Concatenate multiple sub-SDRs into one wider SDR, offsetting each
-- sub-range's bit indices by the cumulative width of the fields
-- before it. This is how multi-field encoders (htm.core style) are
-- built: encode each field independently, then concatenate.
encodeConcat :: [SDR] -> SDR
encodeConcat sdrs = SDR totalWidth allBits
  where
    widths     = map sdrWidth sdrs
    offsets    = scanl (+) 0 widths
    totalWidth = sum widths
    allBits    = IS.unions
      [ IS.map (+ off) (sdrBits s)
      | (s, off) <- zip sdrs offsets
      ]

--------------------------------------------------------------------------------
-- Encoder contracts (roadmap section 7)
--------------------------------------------------------------------------------

-- | An encoder that states its similarity-vs-distance curve as part of
-- its interface. Laws (checked by "Data.SDR.Laws" and the test suite):
--
--   * __Contract__: for all a, b, with d = 'domainDistance' e a b,
--     @lo <= overlap (encodeWith e a) (encodeWith e b) <= hi@ where
--     @(lo, hi) = overlapBounds e d@.
--
--   * __Monotonicity up to resolution__: if
--     @domainDistance e a c >= domainDistance e a b + encoderResolution e@
--     then @overlap (E a) (E c) <= overlap (E a) (E b)@.
--
-- Strict monotonicity ("overlap is non-increasing in distance" for
-- every triple) is false for any bucketing encoder: with resolution r,
-- b at distance 0.3r on one side of a can land in the next bucket while
-- c at 0.4r on the other side stays in a's bucket. The law therefore
-- allows slack of one resolution step, which is the strongest version
-- that bucketing encoders can satisfy.
class MetricEncoder e where
  type Domain e
  encodeWith        :: e -> Domain e -> SDR
  domainDistance    :: e -> Domain e -> Domain e -> Double
  -- | Inclusive (min, max) overlap guaranteed at a given distance.
  overlapBounds     :: e -> Double -> (Int, Int)
  -- | Distance below which the encoder cannot resolve differences.
  encoderResolution :: e -> Double

-- | Contiguous-block scalar encoder over a closed range [lo, hi].
-- Values outside the range are clamped. Width n, w active bits,
-- n − w + 1 buckets, resolution r = (hi − lo) / (n − w).
--
-- Contract: |E(a) ∩ E(b)| = max 0 (w − |bucket a − bucket b|), and the
-- bucket difference is ⌊|a−b|/r⌋ or ⌈|a−b|/r⌉. So overlap lies in
--
-- > [max 0 (w − ⌈|a−b|/r⌉), max 0 (w − ⌊|a−b|/r⌋)]
--
-- which contains the roadmap's max(0, w − ⌈|a−b|/r⌉) as its lower end.
data ScalarEncoder = ScalarEncoder
  { seWidth  :: !Int
  , seActive :: !Int
  , seMin    :: !Double
  , seMax    :: !Double
  } deriving (Eq, Show)

-- | Smart constructor; requires 0 < w < n and lo < hi.
mkScalarEncoder :: Int -> Int -> Double -> Double -> ScalarEncoder
mkScalarEncoder n w lo hi
  | w <= 0 || w >= n = error "SDR.mkScalarEncoder: need 0 < w < n"
  | not (lo < hi)    = error "SDR.mkScalarEncoder: need lo < hi"
  | otherwise        = ScalarEncoder n w lo hi

-- | Resolution r = (hi − lo) / (n − w): the value span of one bucket.
scalarResolution :: ScalarEncoder -> Double
scalarResolution e = (seMax e - seMin e) / fromIntegral (seWidth e - seActive e)

-- | Bucket index in [0, n − w].
scalarBucket :: ScalarEncoder -> Double -> Int
scalarBucket e x =
  let maxB = seWidth e - seActive e
      raw  = floor ((clampScalar e x - seMin e) / scalarResolution e)
  in max 0 (min maxB raw)

clampScalar :: ScalarEncoder -> Double -> Double
clampScalar e = max (seMin e) . min (seMax e)

encodeScalar :: ScalarEncoder -> Double -> SDR
encodeScalar e x =
  let b = scalarBucket e x
  in mkSDRUnsafe (seWidth e) (IS.fromDistinctAscList [b .. b + seActive e - 1])

instance MetricEncoder ScalarEncoder where
  type Domain ScalarEncoder = Double
  encodeWith = encodeScalar
  -- Distance is measured after clamping, since that is what the
  -- encoder actually sees.
  domainDistance e a b = abs (clampScalar e a - clampScalar e b)
  overlapBounds e d =
    let w   = seActive e
        q   = d / scalarResolution e
        eps = 1e-9 * max 1 q   -- absorb floating-point error at bucket edges
    in ( max 0 (w - ceiling (q + eps))
       , min w (max 0 (w - floor (q - eps))) )
  encoderResolution = scalarResolution
