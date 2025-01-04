{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.GraphViz
import Data.GraphViz.Attributes.Complete
import Data.Graph.Inductive

import Options.Applicative
import GHC.Generics (Generic)

-- Type that holds the command-line options
data Options = Options
  { fileName   :: String
  , outputName :: Maybe String
  , outputType :: Maybe String
  } deriving (Show, Generic)

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

main :: IO ()
main = do 
  options <- execParser opts
  putStrLn "Parsed options: "
  print options
  let nodes' = [(1, "A"), (2, "B"), (3, "C")]
  let edges' = [(1, 2, "AB"), (2, 3, "BC")]
  let graph = mkGraph nodes' edges' :: Gr String String
  let params :: GraphvizParams Node String String () String
      params = defaultParams { 
        isDirected = True
      , fmtNode = \(_, label) -> [toLabel label, Shape Circle]
      , fmtEdge = \(_, _, label) -> [toLabel label]
      }
  let dotGraph = graphToDot params graph
  _ <- runGraphviz dotGraph Png "output.png"
  putStrLn "Created output file called 'output.png"
