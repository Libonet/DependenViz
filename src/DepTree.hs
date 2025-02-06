module DepTree (module DepTree) where

import qualified Data.GraphViz.Attributes as GV
import qualified Data.GraphViz.Attributes.Colors as GC
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

getNodeAttributes :: ProjectAttributes -> [GV.X11Color] -> NodeMap -> RevMap -> Int -> GV.Attributes
getNodeAttributes pAttrs colorSpace nodeMap revMap i = case colorBy pAttrs of
  All kind      -> getColorFrom colorSpace i : nodeAttrs
  Clusters kind -> getColorFrom colorSpace (fst $ getRank nodeMap revMap i) : nodeAttrs
  where
    nodeAttrs = getAttributes $ snd $ (Map.!) nodeMap i

getColorFrom :: [GV.X11Color] -> Int -> GV.Attribute
getColorFrom colorSpace i = GV.fillColor $ colorSpace !!! (i-1)

-- safe indexing
(!!!) :: [GV.X11Color] -> Int -> GV.X11Color
[] !!! i = GV.LightBlue
xs !!! i = xs !! (i `mod` length xs)

getColorSpace :: ProjectAttributes -> Int -> Int -> IO [GV.X11Color]
getColorSpace pAttrs nodeCount maxRank = 
  case colorBy pAttrs of
    All kind      -> checkKind kind nodeCount
    Clusters kind -> checkKind kind maxRank
  where
    checkKind kind amount = case kind of
      Random        -> sequence $ generateRandom amount
      FromList list -> return list
      Only color    -> return [color]
    generateRandom 0 = []
    generateRandom i = randomColor : generateRandom (i-1)
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

getRank :: NodeMap -> RevMap -> Int -> (Int, NodeMap)
getRank nodeMap revMap i =
  let (name,attrs) = (Map.!) nodeMap i
  in case rank attrs of
    Just rank -> (rank, nodeMap)
    Nothing ->
      let (maxDeps, newMap) = maxDepsRank (depends attrs) nodeMap revMap
          newRank = max 1 $ maxDeps + 1
          newMap' = Map.insert i (name, addRank (Just newRank) attrs) newMap
      in (newRank, newMap')


maxDepsRank :: [Name] -> NodeMap -> RevMap -> (Int, NodeMap)
maxDepsRank [] nodeMap revMap = (0, nodeMap)
maxDepsRank deps nodeMap revMap = foldr (\name (acc,currMap) -> let (rank, newMap) = rankByName name currMap
                                                            in (max rank acc, newMap)) (0, nodeMap) deps
  where
    rankByName str currMap = getRank currMap revMap (fst $ (Map.!) revMap str)

findMaxRank :: NodeMap -> RevMap -> (Int, NodeMap)
findMaxRank nodeMap revMap = findMaxRank' nodeMap revMap (Map.toList nodeMap) 0

findMaxRank' :: NodeMap -> RevMap -> [(Int, (String, Attributes))] -> Int -> (Int, NodeMap)
findMaxRank' nodeMap revMap [] acc                   = (acc, nodeMap)
findMaxRank' nodeMap revMap ((i,(name,attr)):xs) acc = let (rank, newMap) = getRank nodeMap revMap i
                                                       in findMaxRank' newMap revMap xs (max rank acc)



