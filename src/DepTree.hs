module DepTree where

import Data.GraphViz.Attributes (X11Color)

type Name = String
data Project = Pr Name [Node]

-- Node: Name, attribute list, dependency list
data Node = N Name [Attribute]

data Attribute = Rank Int | Color X11Color | Depends [Name]
