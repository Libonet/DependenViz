module Main where

import Data.Char
import Data.Map (Map)

import Parse (deps_parse, ParseResult(..))
import DepTree (Project(..))
import GraphInspection

import Data.GraphViz
import Data.Graph.Inductive

import Options.Applicative
import System.Exit (exitFailure)
import Data.GraphViz.Attributes.Complete (Attribute(FontSize, LabelLoc), VerticalPlacement (VTop))

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
  putStrLn "Parsed options: "
  print options

  file <- readFile $ fileName options
  let parse_result = deps_parse file
  (Pr name node_list) <- case parse_result of
      Failed err -> do putStrLn err
                       putStrLn "Failed to parse the input file"
                       exitFailure
      Ok project -> return project

  -- Create the Graph
  let nodes' = [(1, "A"), (2, "B"), (3, "C")]
  let edges' = [(1, 2, "AB"), (2, 3, "BC")]
  let graph = mkGraph nodes' edges' :: Gr String String
  let params :: GraphvizParams Node String String () String
      params = defaultParams {
        isDirected = True
      , globalAttributes = [
          GraphAttrs [toLabel name, LabelLoc VTop, FontSize 20]
      ]
      , fmtNode = \(_, label) -> [toLabel label, shape BoxShape]
      , fmtEdge = \(_, _, eLabel) -> [toLabel eLabel]
      }
  let dotGraph = graphToDot params graph

  let (kind, ext) = getKind options
  -- add file extension to output name
  let output = getOutput options ++ "." ++ ext

  -- Generate the visualization
  _ <- runGraphviz dotGraph kind output
  putStrLn $ "Created output file called " ++ output



