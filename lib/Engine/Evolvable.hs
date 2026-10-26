{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeOperators     #-}

module Engine.Evolvable
  ( Evolvable(..)
  ) where

import GHC.Generics ( Generic(..), K1(K1), M1(M1), type (:*:)(..) )
import System.Random (StdGen, randomR)

class Evolvable a where
  -- | Perturb every field by up to +/- rate.
  mutate    :: Float -> StdGen -> a -> (a, StdGen)
  -- | Field-by-field coin-flip crossover between two parents.
  crossover :: StdGen -> a -> a -> (a, StdGen)

  default mutate
    :: (Generic a, GEvolvable (Rep a))
    => Float -> StdGen -> a -> (a, StdGen)
  mutate rate gen x =
    let (rep', gen') = gmutate rate gen (from x)
    in (to rep', gen')

  default crossover
    :: (Generic a, GEvolvable (Rep a))
    => StdGen -> a -> a -> (a, StdGen)
  crossover gen x y =
    let (rep', gen') = gcrossover gen (from x) (from y)
    in (to rep', gen')

-- Generic machinery -----------------------------------------------------

class GEvolvable f where
  gmutate    :: Float -> StdGen -> f p -> (f p, StdGen)
  gcrossover :: StdGen -> f p -> f p -> (f p, StdGen)

-- Strip constructor/selector metadata, recurse into what's inside.
instance GEvolvable f => GEvolvable (M1 i c f) where
  gmutate :: forall k (f :: k -> *) i (c :: Meta) (p :: k).
GEvolvable f =>
Float -> StdGen -> M1 i c f p -> (M1 i c f p, StdGen)
  gmutate rate gen (M1 x) =
    let (x', gen') = gmutate rate gen x in (M1 x', gen')
  gcrossover gen (M1 x) (M1 y) =
    let (z, gen') = gcrossover gen x y in (M1 z, gen')

-- Product of fields: thread the generator through left, then right.
instance (GEvolvable f, GEvolvable g) => GEvolvable (f :*: g) where
  gmutate rate gen (x :*: y) =
    let (x', gen')  = gmutate rate gen x
        (y', gen'') = gmutate rate gen' y
    in (x' :*: y', gen'')
  gcrossover gen (x1 :*: y1) (x2 :*: y2) =
    let (x', gen')  = gcrossover gen x1 x2
        (y', gen'') = gcrossover gen' y1 y2
    in (x' :*: y', gen'')

-- Leaf: an actual Float field.
instance GEvolvable (K1 i Float) where
  gmutate rate gen (K1 x) =
    let (delta, gen') = randomR (-rate, rate) gen
    in (K1 (x + delta), gen')
  gcrossover gen (K1 x) (K1 y) =
    let (pick, gen') = randomR (0 :: Int, 1) gen
    in (K1 (if pick == 0 then x else y), gen')
