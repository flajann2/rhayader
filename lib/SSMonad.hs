{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MonoLocalBinds #-}

-- TODO: Move this into Engine.NEAT

module SSMonad ( module RealNum
               , module Control.Monad.Trans.State
               , module Data.Semigroup
               , module Data.Complex
               , liftIO
               , Config(..)
               , getConfig
               , nextRandom
               , SS(..)
               , nxi
               , Random(..)
               ) where

import Control.Monad.Trans.State
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Semigroup
import Data.Complex (Complex)
import System.Random ( StdGen, Random(randomR), newStdGen )
import RealNum 

import Engine.NEAT.Genetics.GeneTypes  ( NType(Inhibitory, Regular) )
import Engine.NEAT.Evolution.GoalTypes ( Goal(..) )

default (Double)

data Config a = Config { population_size   :: Int
                     , neuron_types      :: [NType a]
                     , goal              :: Goal
                     , sequence_number   :: Int
                     , innovation_number :: Int
                     , num_inputs        :: Int
                     , num_outputs       :: Int
                     , rng               :: IO StdGen -- use nextRandom instead
                     , max_weight        :: a
                     , request_port      :: Int
                     , start_in_port     :: Int
                     , end_in_port       :: Int
                     , start_out_port    :: Int
                     , end_out_port      :: Int
                     , base_in_address   :: String
                     , base_out_address  :: String
                     } 

instance Show a => Show (Config a) where
  show (Config popize
               nt
               goal
               snum
               inum
               ninp
               nout
               _rng
               maxw
               rport
               stin
               endin
               stout
               endout
               basein
               baseout
       ) =  " population_size: "   <> show popize  
         <> " neuron_types: "      <> show nt
         <> " goal: "              <> show goal
         <> " sequence_number: "   <> show snum
         <> " innovation_number: " <> show inum
         <> " num_inputs: "        <> show ninp
         <> " num_outputs: "       <> show nout
         <> " rng: "               <> show rngShow
         <> " max_weight: "        <> show maxw
         <> " request_port: "      <> show rport
         <> " start_in_port: "     <> show stin
         <> " end_in_port: "       <> show endin
         <> " start_out_port: "    <> show stout
         <> " end_out_port: "      <> show endout
         <> " base_in_address: "   <> show basein
         <> " base_out_address: "  <> show baseout
    where
      rngShow = "<IO StdGen>"

newtype SS r a = SS { runSS :: StateT (Config r) IO a }
             deriving ( Functor
                      , Applicative
                      , Monad
                      , MonadIO )
  
initialConfig :: forall a. (RealNum a, Fractional a) => Config a
initialConfig = Config { population_size   = 100
                       , neuron_types      = [ Regular (\x -> x)
                                             , Inhibitory (\x -> x)
                                             ]
                       , goal              = Goal
                       , sequence_number   = 0
                       , innovation_number = 0
                       , num_inputs        = 10
                       , num_outputs       = 2
                       , rng               = newStdGen
                       , max_weight        = 2.0 :: a
                       , request_port      = 31000
                       , start_in_port     = 31020
                       , end_in_port       = 31499
                       , start_out_port    = 31500
                       , end_out_port      = 31999
                       , base_in_address   = "tcp://127.0.0.1:"
                       , base_out_address  = "tcp://127.0.0.1:"
                       }

getConfig :: SS a (Config a)
getConfig = SS get

updateConfig :: Config a -> SS a ()
updateConfig newconf = SS $ put newconf

-- TODO: The following two monads share similar functionality and should
-- TODO: be DRYed up. Or not bother?
nextSequenceNumber :: SS r Int
nextSequenceNumber = do
  config <- getConfig
  let next' = config.sequence_number
  let uconf = config { sequence_number = next' + 1 }
  updateConfig uconf
  pure next'

nextInnovationNumber :: SS r Int
nextInnovationNumber = do
  config <- getConfig
  let next_innov = config.innovation_number
  let uconf = config {innovation_number = next_innov + 1 }
  updateConfig uconf
  return next_innov

nsi :: SS r Int
nsi = nextSequenceNumber

nxi :: SS r Int
nxi = nextInnovationNumber

nextRandom :: forall a. (RealNum a, Random a) => (a, a) -> SS a a
nextRandom (from, to) = do
  cfg <- getConfig
  rn <- liftIO $ do
    gen <- cfg.rng
    let (randNum, _) = randomR (from, to) gen
    return randNum
  return rn
