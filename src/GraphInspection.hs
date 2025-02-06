module GraphInspection (checkCycles) where

import qualified Data.Graph.Inductive as G
import qualified DepTree as DT

import qualified Data.Map as Map

import System.Exit (exitFailure)

{-
 -  Kahn's algorithm for topological sorting
 -
 -  Remove from the graph the nodes with a 0 in-degree.
 -  If there are no '0 in-degree' nodes left and the graph 
 -  isn't empty then the graph can't be sorted (it has cycles)
 -
 -  So we check for cycles by trying to consume the graph
 -}
kahnAlgorithm :: (G.DynGraph gr, Eq (gr a b)) => gr a b -> gr a b
kahnAlgorithm graph =
  if G.isEmpty graph
  then graph
  else let filtered = G.nfilter (haveDeps graph) graph
       in if filtered == graph
          then filtered
          else kahnAlgorithm filtered

-- Check if a given node has any dependencies
haveDeps :: G.Graph gr => gr a b -> G.Node -> Bool
haveDeps gr = not . null . G.pre gr

-- Use the kahn algorithm to check if a graph has cycles
checkCycles :: (G.DynGraph gr, Eq (gr a b)) => gr a b -> DT.NodeMap -> IO (gr a b)
checkCycles graph nodeMap =
  do let ret = kahnAlgorithm graph
     if G.isEmpty ret
     then return graph
     else do putStrLn "Failed to create the tree! The dependency tree has a cycle"
             putStrLn $ "Nodes in a cycle: " ++ getNodeLabels ret nodeMap
             -- prettyPrint ret
             exitFailure


getNodeLabels :: G.Graph gr => gr a b -> DT.NodeMap -> String
getNodeLabels gr nodeMap =
    let names = map (fst . (Map.!) nodeMap) $ G.nodes gr
    in separateNames names
  where
    separateNames [] = []
    separateNames [x] = x
    separateNames (x:xs) = x ++ ", " ++ separateNames xs

