{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
module Engine.NEAT.Genetics.GeneTypes where

import Data.IORef ( IORef )
import RealNum ()
import Engine.NEAT.Izhikevich

data NType a = Pyramidal  { activation     :: a -> a
                          , depolarization :: a }
             | Purkinje   { activation     :: a -> a
                          , depolarization :: a
                          , rate           :: a }
             | Regular    { activation     :: a -> a }
             | Inhibitory { activation     :: a -> a }
             | Izhikevich { activation     :: a -> a
                          , parms          :: IzhikevichParams a
                          , state          :: IORef (NeuronState a)
                            -- ^ ever-changing state per tick
                          }

instance Show a => Show (NType a) where
    show :: NType a -> String
    show (Pyramidal _ dep)     = "Pyramidal with depolarization: " ++ show dep
    show (Purkinje _ dep rate) = "Purkinje with depolarization: " ++ show dep ++ ", rate: " ++ show rate
    show (Regular _)           = "Regular Neuron"
    show (Inhibitory _)        = "Inhibitory Neuron"

instance Eq a => Eq (NType a) where
    (==) :: NType a -> NType a -> Bool
    (Pyramidal _ dep1)      == (Pyramidal _ dep2)      = dep1 == dep2
    (Purkinje _ dep1 rate1) == (Purkinje _ dep2 rate2) = dep1 == dep2 && rate1 == rate2
    (Regular _)             == (Regular _)             = True
    (Inhibitory _)          == (Inhibitory _)          = True
    _ == _ = False  -- Different constructors are not equal
