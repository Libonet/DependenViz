module GraphInspection where

import qualified Data.Graph.Inductive as G

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
  else let filtered = G.gfiltermap haveDeps graph
       in if filtered == graph
          then graph
          else kahnAlgorithm filtered


haveDeps :: G.Context a b -> G.MContext a b
haveDeps n@(toNode, _, _, _) = 
  if null toNode 
  then Nothing 
  else Just n
