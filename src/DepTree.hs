module DepTree (module DepTree) where

import qualified Data.GraphViz.Attributes as GV
import qualified Data.GraphViz.Attributes.Colors as GC

import Data.Array.IArray (Ix, Array, (!), (!?), listArray, array)
import qualified Data.Map as Map

import qualified Data.Graph.Inductive as GI

import System.Random (randomIO)

-- Project

data Project = Pr ProjectAttributes [Node]

data ProjectAttributes = PrAttrs
  { name :: Name
  , colorBy :: ColorRange
  }

type Name = String
data ColorRange = All ColorType | Clusters ColorType
data ColorType = Random | Only GV.X11Color | FromList [GV.X11Color]

newProject = PrAttrs {name="", colorBy=All $ Only GV.LightBlue}
addName name attr = attr { name = name}
addColorBy colorRange attr = attr { colorBy = colorRange }

-- Search for the node's attributes depending on the project's settings
getNodeAttributes :: ProjectAttributes -> Array Int GV.X11Color -> RankMap -> NodeMap -> Int -> GV.Attributes
getNodeAttributes pAttrs colorSpace rankMap nodeMap i = case colorBy pAttrs of
  All kind      -> getColorFromColorSpace colorSpace i : nodeAttrs
  Clusters kind -> getColorFromColorSpace colorSpace ((Map.!) rankMap i) : nodeAttrs
  where
    nodeAttrs = getAttributes $ snd $ (Map.!) nodeMap i

getColorFromColorSpace :: Array Int GV.X11Color -> Int -> GV.Attribute
getColorFromColorSpace colorSpace i = case colorSpace !? i of
  Nothing -> let len = length colorSpace
             in GV.fillColor $ colorSpace ! ((i `mod` len) + 1)
  Just c  -> GV.fillColor c

getColorSpace :: ProjectAttributes -> Int -> Int -> IO (Array Int GV.X11Color)
getColorSpace pAttrs nodeCount maxRank = 
  case colorBy pAttrs of
    All kind      -> checkKind kind nodeCount
    Clusters kind -> checkKind kind maxRank
  where
    checkKind :: ColorType -> Int -> IO (Array Int GV.X11Color)
    checkKind kind amount = case kind of
      Random        -> sequence $ array (1,amount) [(i, randomColor) | i <- [1..amount]]
      FromList list -> return $ listArray (1, length list) list
      Only color    -> return $ array (1,1) [(1,color)]
    randomColor :: IO GV.X11Color
    randomColor = do index <- randomIO :: IO Int
                     return $ toEnum $ (index `mod` fromEnum (maxBound :: GV.X11Color))
                                       + fromEnum (minBound :: GV.X11Color)

-- Node: Name, attribute list
data Node = N Name Attributes

data Attributes = Attrs
  { color :: Maybe GV.X11Color
  , rank :: Maybe Int
  , depends :: [Name]
  }

getAttributes attr =
    case color attr of
    Just c -> [GV.fillColor c]
    Nothing -> []

addColor color attr = attr { color = Just color }
addRank rank attr   = attr { rank = rank }
addDeps deps attr   = attr { depends = deps }

newAttribute = Attrs {color=Nothing, rank=Nothing, depends=[]}

type NodeMap = Map.Map Int (Name, Attributes)
type RevMap = Map.Map Name (Int, Attributes)

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

type RankMap = Map.Map Int Int

-- To find the rank of a node we need to check if 
getRank :: RankMap -> NodeMap -> RevMap -> Int -> (Int, RankMap)
getRank rankMap nodeMap revMap i =
  case (Map.!?) rankMap i of
    Nothing -> 
      let (name,attrs) = (Map.!) nodeMap i
      in case rank attrs of
        Just rank -> 
          let (maxDRank, rankMap') = maxDepsRank (depends attrs) rankMap nodeMap revMap
          in  if rank > maxDRank
              then (rank, Map.insert i rank rankMap')
              else (maxDRank + 1, Map.insert i (maxDRank + 1) rankMap')
        Nothing ->
          let (maxDRank, rankMap') = maxDepsRank (depends attrs) rankMap nodeMap revMap
              newRank = maxDRank + 1
          in (newRank, Map.insert i newRank rankMap')
    Just rank -> (rank, rankMap)


maxDepsRank :: [Name] -> RankMap -> NodeMap -> RevMap -> (Int, RankMap)
maxDepsRank [] rankMap _ _ = (0, rankMap)
maxDepsRank deps rankMap nodeMap revMap = 
    foldr (\name (acc,currMap) -> let (rank, newMap) = rankByName name currMap
                                  in (max rank acc, newMap)) (0, rankMap) deps
  where
    rankByName str currMap = getRank currMap nodeMap revMap (fst $ (Map.!) revMap str)

findMaxRank :: NodeMap -> RevMap -> (Int, RankMap)
findMaxRank nodeMap revMap = findMaxRank' Map.empty nodeMap revMap (Map.toList nodeMap) 0

findMaxRank' :: RankMap -> NodeMap -> RevMap -> [(Int, (String, Attributes))] -> Int -> (Int, RankMap)
findMaxRank' rankMap _ _ [] acc                   = (acc, rankMap)
findMaxRank' rankMap nodeMap revMap ((i,(name,attr)):xs) acc = 
  let (rank, rankMap') = getRank rankMap nodeMap revMap i
  in findMaxRank' rankMap' nodeMap revMap xs (max rank acc)



