{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}
module DepTree where

import qualified Data.GraphViz.Attributes as GV
import qualified Data.Map as Map

import qualified Data.Graph.Inductive as GI

type Name = String
data Project = Pr Name [Node]

-- Node: Name, attribute list
data Node = N Name Attributes

getNames :: [Node] -> [String]
getNames [] = []
getNames ((N name _):xs) = name : getNames xs

data Attributes = Attrs
  { color :: GV.X11Color
  , rank :: Maybe Int
  , depends :: [Name]
  }

getAttributes attr = [
    GV.fillColor (color attr)
  ]

addColor color attr = attr { color = color }
addRank rank attr   = attr { rank = rank }
addDeps deps attr   = attr { depends = deps }

newAttribute = Attrs {color=GV.LightBlue, rank=Nothing, depends=[]}

type NodeMap = Map.Map Int (Name, Attributes)
type RevMap = Map.Map Name (Int, Attributes)

-- type Env = (NodeMap, RevMap)
-- newtype DepGraph a = DG { runDG :: Env -> (a, Env) }
--
-- instance Monad DepGraph where 
--   return x = DG (x,) -- TupleSection: \st -> (x,st) = (x,)
--   m >>= f = DG (\env -> do (x, env') <- runDG m env
--                            runDG (f x) env')

-- -- search for a Node's info by it's ID
-- lookById id = DG (\env -> lookById' id env)
--     where lookById' :: Int -> Env -> (Name, Attributes)
--           lookById' id (nm,_) = (Map.!) nm id
--
-- -- find a Node's ID then search for it's info
-- lookByName name = DG (\env -> lookByName' name env)
--     where lookByName' :: Name -> Env -> (Name, Attributes)
--           lookByName' name (nm,rm) = lookById' ((Map.!) rm name) nm
--           lookById' id nm = (Map.!) nm id
--
-- update id newVal =  DG (\(nm,rm) -> let (name, oldAttr) = (Map.!) nm id
--                                   in return ((), (Map.insert id (name,newVal) nm,rm)))

createGraph :: [Node] -> (GI.Gr Name Name, NodeMap, RevMap)
createGraph nodeList =
  let enumerateNodes = zipWith (\i (N name attrs) -> (i, (name,attrs))) [1..] (reverse nodeList)
      nodeMap = Map.fromList enumerateNodes
      revMap = Map.fromList $ map (\(i,(name,attrs)) -> (name,(i,attrs))) enumerateNodes
      nodes = map (\(i,(name,attrs)) -> (i,name)) enumerateNodes
      edges = findEdges revMap nodes
  in (GI.mkGraph nodes edges, nodeMap, revMap)

findEdges :: RevMap -> [(Int,Name)] -> [(Int,Int,Name)]
findEdges revMap = concatMap (getEdges revMap)

getEdges :: RevMap -> (Int,Name) -> [(Int,Int,Name)]
getEdges revMap (i,name) = let deps = getDepends revMap name
                           in map (\dep -> (fst ((Map.!) revMap dep), i, "")) deps

getDepends :: RevMap -> Name -> [Name]
getDepends revMap name = depends $ snd $ (Map.!) revMap name

getRank :: NodeMap -> RevMap -> Int -> (Int, NodeMap)
getRank nodeMap revMap i =
  let (name,attrs) = (Map.!) nodeMap i
  in case rank attrs of
    Just rank -> (rank, nodeMap)
    Nothing ->
      let (maxDeps, newMap) = maxRank (depends attrs) nodeMap revMap
          newRank = max 1 $ maxDeps + 1
          newMap' = Map.insert i (name, addRank (Just newRank) attrs) newMap
      in (newRank, newMap')


maxRank :: [Name] -> NodeMap -> RevMap -> (Int, NodeMap)
maxRank [] nodeMap revMap = (0, nodeMap)
maxRank deps nodeMap revMap = foldr (\name (acc,currMap) -> let (rank, newMap) = rankByName name currMap
                                                            in (max rank acc, newMap)) (0, nodeMap) deps
  where
    rankByName str currMap = getRank currMap revMap (fst $ (Map.!) revMap str)

findMaxRank :: NodeMap -> RevMap -> (Int, NodeMap)
findMaxRank nodeMap revMap = findMaxRank' nodeMap revMap (Map.toList nodeMap) 0

findMaxRank' :: NodeMap -> RevMap -> [(Int, (String, Attributes))] -> Int -> (Int, NodeMap)
findMaxRank' nodeMap revMap [] acc                   = (acc, nodeMap)
findMaxRank' nodeMap revMap ((i,(name,attr)):xs) acc = let (rank, newMap) = getRank nodeMap revMap i
                                                       in findMaxRank' newMap revMap xs (max rank acc)
                                                       

