{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MonoLocalBinds #-}

module Engine.NEAT.Genetics.Critters where

import SSMonad
    ( getConfig
    , nextRandom
    , nxi
    , liftIO
    , Config(num_outputs, num_inputs)
    , SS
    , Random(..)) 
import RealNum
import Engine.NEAT.Genetics.Genes
    ( mkRegular, NType, Connection(..), Node(..), Role(..) )
--- import Control.Monad (mapM)
--- import GHC.Cmm.Utils (mkRODataLits)
--- import GHC.Builtin.Uniques (mkRegClassUnique)

-- |Critter comprises the nodes, which are really the neurons,
--  and the indices inputs, outputs, and hidden, which defines
--  the functionality of the nodes (neurons)
--  We also have connections, which are the connections
--  among the nodes.
data Critter a = Critter { nodes        :: [Node a]
                       , inputs         :: [Int] -- ^ indices of the input nodes
                       , outputs        :: [Int] -- ^ indices of the output nodes
                       , hidden         :: [Int] -- ^ indices of the hidden nodes
                       , connections    :: [Connection a]
                       , number_inputs  :: Int
                       , number_outputs :: Int
                       } deriving Show

class Eval a where
  ecrit :: [a] -> SS a [a]
  epop  :: [a] -> SS a [[a]]

node :: NType a -> Role -> Node a
node nt r = Node { ntype = nt
                 , role = r }

rwei :: (RealNum a, Random a) => SS a a
rwei = nextRandom (0.0, 5.0)

conn :: (RealNum a, Random a) => Int -> Int -> SS a Int -> SS a (Connection a)
conn in' out' innov = do
  innov' <- innov
  rwei'  <- rwei
  return $ Connection { innovation = innov'
                      , node_in    = in'
                      , node_out   = out'
                      , weight     = rwei'
                      , enabled    = True
                      }

mkCritter :: [Node a] -> [SS a (Connection a)] -> SS a (Critter a)
mkCritter ns cs = do
  cfg <- getConfig
  cs' <- mapM id cs
  let crit = Critter { nodes          = ns
                     , connections    = cs'
                     , number_inputs  = cfg.num_inputs
                     , number_outputs = cfg.num_outputs
                     , inputs         = findInputs
                     , outputs        = findOutputs
                     , hidden         = findHidden
                     }
  return crit
    where
      findInputs :: [Int]
      findInputs = [i | (n, i) <- zip ns [0..], n.role == Input] 
      
      findOutputs :: [Int]
      findOutputs = [i | (n, i) <- zip ns [0..], n.role == Output]

      findHidden :: [Int]
      findHidden = [i | (n, i) <- zip ns [0..], n.role == Hidden]

genCritter :: forall a. (RealNum a, Random a) => SS a (Critter a)
genCritter = do
  cfg <- getConfig
  let conns = genconn cfg.num_inputs cfg.num_outputs
  _ <- liftIO $ putStrLn $ show cfg.num_inputs
  _ <- liftIO $ putStrLn $ show cfg.num_outputs
  _ <- liftIO $ putStrLn $ show $ length conns
  crit <- mkCritter ((nodes (cfg.num_inputs)  Input [])
                  ++ (nodes (cfg.num_outputs) Output [])) conns
  return crit
  where
    nodes :: Int -> Role -> [Node a] -> [Node a]
    nodes 0 _ ns = ns
    nodes cnt r ns = nodes (cnt - 1) r $ (node mkRegular r) : ns

    genconn :: Int -> Int -> [SS a (Connection a)]
    genconn ins outs = [conn i j nxi | i <- [0..(ins-1)], j <- [ins..(ins+outs-1)]]
