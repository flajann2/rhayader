{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}

module Engine.NEAT.Genetics.Genes ( NType(..)
                                  , Role(..)
                                  , Node(..)
                                  , Connection(..)
                                  , mkPyramidal
                                  , mkPurkinje
                                  , mkRegular
                                  , mkInhiborty
                                  ) where
-- import qualified Data.HashMap.Strict as HM
-- import Data.Hashable (Hashable)
-- import SSMonad
import Engine.NEAT.Genetics.GeneTypes

-- neuron constructor functions
mkPyramidal'      :: (a -> a) -> a -> NType a
mkPyramidal' f d  = Pyramidal f d
mkPurkinje'       :: (a -> a) -> a -> a -> NType a
mkPurkinje' f d r = Purkinje f d r
mkRegular'        :: (a -> a) -> NType a
mkRegular' f      = Regular f
mkInhiborty'      :: (a -> a) -> NType a
mkInhiborty' f    = Inhibitory f

-- neuron constructor functions with sigmoid and other activations
-- TODO: modify the activation functions to what they should be.
mkPyramidal    :: Floating a => a -> NType a
mkPyramidal d  = mkPyramidal' (\x -> 1 / (1 + exp (-x))) d
mkPurkinje     :: Floating a => a -> a -> NType a
mkPurkinje d r = mkPurkinje' (\x -> 1 / (1 + exp (-x))) d r
mkRegular      :: Floating a => NType a
mkRegular      = mkRegular' (\x -> 1 / (1 + exp (-x)))
mkInhiborty    :: Floating a => NType a
mkInhiborty    = mkInhiborty' (\x -> 1 / (1 + exp (-x)))

-- TODO: remove this if we dont need the Eq

data Role = Input
          | Output
          | Hidden
          deriving (Show, Eq)
-- |
data Node a = Node { ntype    :: NType a
                   , role     :: Role
                   } deriving (Show, Eq)

-- | Connection between nodes (neurons) which are evolvable.
data Connection a = Connection { innovation :: Int
                                , node_in    :: Int
                                , node_out   :: Int
                                , weight     :: a
                                , enabled    :: Bool
                                } deriving Show
instance Eq a => Eq (Connection a) where
  (==) :: Connection a -> Connection a -> Bool
  (Connection _innov1 nin1 nout1 w1 en1)
    == (Connection _innov2 nin2 nout2 w2 en2) = nin1 == nin2
    && nout1 == nout2
    && w1 == w2
    && en1 == en2
