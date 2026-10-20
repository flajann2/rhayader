module Data.SDR.Core where

import qualified Data.IntSet as IS
import Data.IntSet (IntSet)

--------------------------------------------------------------------------------
-- Core type
--------------------------------------------------------------------------------

data SDR = SDR
  { sdrWidth :: {-# UNPACK #-} !Int   -- ^ dimensionality, n
  , sdrBits  :: !IntSet               -- ^ active bit indices, subset of [0, sdrWidth)
  } deriving (Eq, Ord, Show)

-- | Construct an SDR, checking that all active bits fall in range.
-- Errors on out-of-range bits. Safe for boundaries where input isn't
-- already known-good (e.g. deserialization, encoder output).
mkSDR :: Int -> IntSet -> SDR
mkSDR w bits
  | IS.null bits = SDR w bits
  | IS.findMin bits >= 0 && IS.findMax bits < w = SDR w bits
  | otherwise = error "SDR.mkSDR: bit index out of range"

-- | Construct an SDR without the range check, for hot paths where the
-- caller has already validated invariants upstream.
mkSDRUnsafe :: Int -> IntSet -> SDR
mkSDRUnsafe = SDR

emptySDR :: Int -> SDR
emptySDR w = SDR w IS.empty

-- | Number of active bits, w_x = |x|.
sdrWeight :: SDR -> Int
sdrWeight = IS.size . sdrBits
{-# INLINE sdrWeight #-}

requireSameWidth :: SDR -> SDR -> a -> a
requireSameWidth a b x
  | sdrWidth a == sdrWidth b = x
  | otherwise = error "SDR: width mismatch"

