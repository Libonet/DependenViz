module Main where

import Data.Char
import qualified Data.Map as Map

import Parse (deps_parse, ParseResult(..))
import qualified DepTree as DT
import qualified GraphInspection as GI

import Data.GraphViz
import Data.Graph.Inductive

import Options.Applicative hiding (style)
import System.Exit (exitFailure)

-- Type that holds the command-line options
data Options = Options
  { fileName   :: String
  , outputName :: Maybe String
  , outputType :: Maybe String
  } deriving Show

optionsParser :: Parser Options
optionsParser = Options
  <$> argument str
      ( metavar "FILENAME"
     <> help "Name of the file to process"
      )
  <*> optional (strOption
      ( long "output"
     <> short 'o'
     <> metavar "File"
     <> help "Optional name of output file. Default (FILENAME)"
      ))
  <*> optional (strOption
      ( long "type"
     <> short 't'
     <> metavar "Extension"
     <> help "Optional extension of output file. Default 'png'"
      ))

-- Parser info with description
opts :: ParserInfo Options
opts = info (optionsParser <**> helper)
    ( fullDesc
   <> progDesc "A program to visualize dependency trees"
   <> header "DependenViz"
    )

getOutput :: Options -> String
getOutput options = case outputName options of
  Nothing   -> dropExtension $ fileName options
    where
      dropExtension = takeWhile (/= '.')
  Just name -> name

getKind :: Options -> (GraphvizOutput, String)
getKind options = case outputType options of
  Nothing  -> (Png, "png")
  Just string -> case map toLower string of
              n@"png"  -> (Png, n)
              n@"svg"  -> (Svg, n)
              n@"jpg"  -> (Jpeg, n)
              n@"jpeg" -> (Jpeg, n)
              _      -> (Png, "png")

main :: IO ()
main = do
  -- Parse the input
  options <- execParser opts
  -- putStrLn "Parsed options: "
  -- print options

  file <- readFile $ fileName options

  project <- case deps_parse file of
      Failed err -> do putStrLn err
                       putStrLn "Failed to parse the input file!"
                       exitFailure
      Ok project -> return project

  dotGraph <- createDotGraph project

  let (kind, ext) = getKind options
  -- add file extension to output name
  let output = getOutput options ++ "." ++ ext

  -- Generate the visualization
  output_path <- runGraphviz dotGraph kind output
  putStrLn $ "Created output file: " ++ output_path

createDotGraph :: DT.Project -> IO (DotGraph Data.Graph.Inductive.Node)
createDotGraph (DT.Pr pName nodeList) = do
  let (graph', nodeMap, revMap) = DT.createGraph nodeList

  -- check if the graph has no cycles
  graph <- do let ret = GI.kahnAlgorithm graph'
              if isEmpty ret
              then return graph'
              else do putStrLn "Failed to create the tree! The dependency tree has a cycle"
                      putStrLn $ "Possible offenders: " ++ getNodeLabels ret nodeMap
                      exitFailure

  let nodeCount = Map.size nodeMap
  -- putStrLn $ "NodeCount = " ++ show nodeCount
  let (maxRank, rankedNodeMap) = DT.findMaxRank nodeMap revMap
  -- putStrLn $ "maxRank = " ++ show maxRank

  -- Add attributes to nodes
  let params :: GraphvizParams Data.Graph.Inductive.Node String String Int String
      params = defaultParams {
        isDirected = True
      , clusterID = Num . Int
      , clusterBy = clustBy nodeCount rankedNodeMap revMap
      , fmtCluster = const [GraphAttrs [rank SameRank, style invis]]
      , fmtNode = \case
          { (0, _)     -> [toLabel pName, shape BoxShape, style bold]
          ; (i, label) -> if i <= nodeCount
                          then [toLabel label, shape BoxShape, style filled]
                               ++ (DT.getAttributes . snd) ((Map.!) rankedNodeMap i)
                          else [style invis]
          }
      -- we make the edges from the title invisible
      , fmtEdge = \(i, _, eLabel) -> if i == 0 || i >= nodeCount then [style invis] else [toLabel eLabel]
      }

  if pName == ""
  then return $ graphToDot params graph
  else
    -- Insert the node that holds the title
    let titledGraph'   = insNode (0, pName) graph
        titledGraph''  = referenceNodes titledGraph' nodeCount maxRank
        titledGraph''' = referenceEdges titledGraph'' nodeCount maxRank
        titledGraph    = invisibleEdges titledGraph''' rankedNodeMap revMap
    in return $ graphToDot params titledGraph

-- repeatFunc :: (a -> a) -> Int -> a -> a
-- repeatFunc _ 0 acc = acc
-- repeatFunc f times acc = repeatFunc f (times-1) (f acc)

getNodeLabels :: Graph gr => gr a b -> DT.NodeMap -> String
getNodeLabels gr nodeMap = 
    let names = map (fst . (Map.!) nodeMap) $ nodes gr
    in separateNames names
  where
    separateNames [] = []
    separateNames [x] = x
    separateNames (x:xs) = x ++ ", " ++ separateNames xs

clustBy :: Int -> DT.NodeMap -> DT.RevMap -> (Int, String) -> NodeCluster Int (Int, String)
clustBy nodeCount nodeMap revMap (n,l) = case n of
  0  -> C 0 $ Data.GraphViz.N (n,l)
  _  -> if n <= nodeCount
        then C (fst $ DT.getRank nodeMap revMap n) $ Data.GraphViz.N (n,l)
        else C (read l) $ Data.GraphViz.N (n,l)

referenceNodes :: Gr String String -> Int -> Int -> Gr String String
referenceNodes graph' nodeCount maxRank = insNodes [(i+nodeCount,show i) | i <- [1..maxRank] ] graph'

referenceEdges :: Gr String String -> Int -> Int -> Gr String String
referenceEdges graph' nodeCount maxRank = insEdges [(i+nodeCount,i+nodeCount+1,show i) | i <- [1..maxRank-1] ] graph'

invisibleEdges :: Gr String String -> DT.NodeMap -> DT.RevMap -> Gr String String
invisibleEdges graph' nodeMap revMap = aux graph' (Map.keys nodeMap)
  where
    aux :: Gr String String -> [Int] -> Gr String String
    aux graph'' []     = graph''
    aux graph'' (i:xs) = if (fst . DT.getRank nodeMap revMap) i == 1 then aux (insEdge (0,i,"") graph'') xs else aux graph'' xs


