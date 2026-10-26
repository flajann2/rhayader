module Engine.NEAT.Population where

import Engine.NEAT.Genetics.Critters
import SSMonad ( SS )
-- import SSNumeric

data Population a = Population { critters :: [Critter a]
                               , epsilon :: [Maybe Float]
                               } deriving Show

genPopulation :: SS a (Population a)
genPopulation = undefined
