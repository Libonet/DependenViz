module Main where

import qualified Data.Map as Map

import Parse (depsParse, ParseResult(..))
import qualified DepTree as DT
import GraphInspection (checkCycles)

import Data.GraphViz
import Data.GraphViz.Attributes.Complete
import Data.Graph.Inductive

import Options.Applicative hiding (style)
import System.Exit (exitFailure)
import Text.Read (readMaybe)

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

getKind :: Options -> IO (GraphvizOutput, String)
getKind options = case outputType options of
  Nothing  -> return (Png, "png")
  Just string -> case readMaybe string of
    Nothing   -> do putStrLn "Invalid extension kind!"
                    exitFailure
    Just kind -> return (kind, string)

main :: IO ()
main = do
  quitWithoutGraphviz "Graphviz doesn't seem to be available. Install it before using DependenViz."

  -- Parse the program's arguments
  options <- execParser opts
  -- putStrLn "Parsed options: "
  -- print options

  file <- readFile $ fileName options

  project <- case depsParse file of
      Failed err -> do putStrLn err
                       putStrLn "Failed to parse the input file!"
                       exitFailure
      Ok project -> return project

  dotGraph <- createDotGraph project

  (kind, ext) <- getKind options
  -- add file extension to output name
  let output = getOutput options ++ "." ++ ext

  -- Generate the visualization
  output_path <- runGraphviz dotGraph kind output
  putStrLn $ "Created output file: " ++ output_path

createDotGraph :: DT.Project -> IO (DotGraph Data.Graph.Inductive.Node)
createDotGraph (DT.Pr pAttrs nodeList) = do
  let (graph', nodeMap, revMap) = DT.createGraph nodeList

  -- check if the graph has no cycles
  graph <- checkCycles graph' nodeMap

  let nodeCount = Map.size nodeMap
  let (maxRank, rankMap) = DT.findMaxRank nodeMap revMap

  colorSpace <- DT.getColorSpace pAttrs nodeCount maxRank

  -- Add attributes to nodes
  let params :: GraphvizParams Data.Graph.Inductive.Node String String Int String
      params = defaultParams {
        isDirected = True
      , globalAttributes = [GraphAttrs [Splines Ortho]]
      , clusterID = Num . Int
      , clusterBy = clustBy nodeCount rankMap
      , fmtCluster = const [GraphAttrs [rank SameRank, style invis]]
      , fmtNode = \case
          { 
          ; (_, "")    -> [style invis]
          ; (0, _)     -> [toLabel (DT.name pAttrs), shape BoxShape, style bold]
          ; (i, label) -> if i <= nodeCount
                          then let nAttrs = DT.getNodeAttributes pAttrs colorSpace rankMap nodeMap i
                               in [toLabel label, shape BoxShape, style filled] ++ nAttrs
                          else [style invis]
          }
      -- we make the edges from the title invisible
      , fmtEdge = \(i, _, eLabel) -> if i == 0 || i >= nodeCount then [style invis] else [toLabel eLabel]
      }

  -- Insert the node that holds the title
  let titledGraph'   = if DT.name pAttrs /= ""
                       then let titledGraph = insNode (0, DT.name pAttrs) graph
                            in  conectTitle titledGraph rankMap
                       else graph
      -- insert the invisible referenceNodes that keep the specified ranks
      titledGraph''  = referenceNodes titledGraph' nodeCount maxRank
      titledGraph''' = referenceEdges titledGraph'' nodeCount maxRank 
  return $ graphToDot params titledGraph'''

-- repeatFunc :: (a -> a) -> Int -> a -> a
-- repeatFunc _ 0 acc = acc
-- repeatFunc f times acc = repeatFunc f (times-1) (f acc)

clustBy :: Int -> DT.RankMap -> (Int, String) -> NodeCluster Int (Int, String)
clustBy nodeCount rankMap (n,l) = case n of
  0  -> C 0 $ Data.GraphViz.N (n,l)
  _  -> if n <= nodeCount
        then C ((Map.!) rankMap n) $ Data.GraphViz.N (n,l)
        else C (read l) $ Data.GraphViz.N (n,l)

referenceNodes :: Gr String String -> Int -> Int -> Gr String String
referenceNodes graph' nodeCount maxRank = insNodes [(i+nodeCount,show i) | i <- [1..maxRank] ] graph'

referenceEdges :: Gr String String -> Int -> Int -> Gr String String
referenceEdges graph' nodeCount maxRank = insEdges [(i+nodeCount,i+nodeCount+1,show i) | i <- [1..maxRank-1] ] graph'

conectTitle :: Gr String String -> DT.RankMap -> Gr String String
conectTitle graph' rankMap = aux graph' (Map.keys rankMap)
  where
    aux :: Gr String String -> [Int] -> Gr String String
    aux graph'' []     = graph''
    aux graph'' (i:xs) = if (Map.!) rankMap i == 1 then aux (insEdge (0,i,"") graph'') xs else aux graph'' xs


