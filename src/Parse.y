{
module Parse where
import DepTree
import Data.GraphViz.Attributes.Colors.X11 ( X11Color(LightBlue) )

import Data.Maybe
import Data.Char
import Text.Read (readMaybe)
}

%monad { P } { thenP } { returnP }
%name parseDeps Start

%tokentype { Token }
%lexer { lexer } { TEOF }

%token
    ':'      { TColon }
    ';'      { TSemiColon }
    ','      { TComma }
    '{'      { TOpen }
    '}'      { TClose }
    '['      { TListOpen }
    ']'      { TListClose }
    '('      { TParenOpen }
    ')'      { TParenClose }
    name     { TPrName }
    all      { TAll }
    clusters { TClusters }
    random   { TRandom }
    only     { TOnly }
    fromList { TFromList }
    INT      { TInt $$ }
    rank     { TRank }
    color    { TColor }
    depends  { TDepends }
    NAME     { TName $$ }
    
%%

{-
-- Name and attributes
(Example; color: all only Blue) Nodes...

-- Only name
(Example2) Nodes...

-- Only attributes
(name: Example3, color: clusters fromList [LightBlue, Blue, Green, LightGreen]) Nodes...
-}

Start :: { Project }
      : '(' ProjectInfo ')' Nodes { Pr $2 $4 }
      | Nodes                { Pr newProject $1 }

----- Project construction -----

ProjectInfo :: { ProjectAttributes }
       : NAME ';' PrAttributes { addName $1 $3 }
       | PrAttributes          { $1 }
       | NAME                  { addName $1 newProject }

PrAttributes :: { ProjectAttributes }
             : PrAttributes1 PrAttribute ',' { $2 ($1 newProject) }
             | PrAttributes1 PrAttribute     { $2 ($1 newProject) }
             | PrAttribute ','               { $1 newProject }
             | PrAttribute                   { $1 newProject }
             | {- empty -}                   { newProject }

PrAttributes1 :: { ProjectAttributes -> ProjectAttributes }
              : PrAttributes1 PrAttribute ',' { $2 . $1 }
              | PrAttribute ','               { $1 }

PrAttribute :: { ProjectAttributes -> ProjectAttributes }
            : name ':' NAME        { addName $3 }
            | color ':' ColorRange { addColorBy $3 }

ColorRange :: { ColorRange }
           : all ColorType      { All $2 }
           | clusters ColorType { Clusters $2 }

ColorType :: { ColorType }
          : random                     { Random }
          | only Color                 { Only $2 }
          | fromList '[' ColorList ']' { FromList (reverse $3) }

ColorList :: { [X11Color] }
          : ColorList ',' Color { $3 : $1 }
          | Color               { [$1] }

Color :: { X11Color }
      : NAME { parseX11Color $1 }

----- Nodes parsing -----

Nodes :: { [Node] }
      : Nodes Node                    { $2 : $1 }
      | {- empty -}                   { [] }

Node :: { Node }
     : NAME '{' Attributes '}'        { N $1 $3 }

-- Attributes are comma separated, optionally with a comma at the end
Attributes :: { Attributes }
           : Attributes1 Attribute ',' { $2 ($1 newAttribute) }
           | Attributes1 Attribute     { $2 ($1 newAttribute) }
           | Attribute ','             { $1 newAttribute }
           | Attribute                 { $1 newAttribute }
           | {- empty -}               { newAttribute }

Attributes1 :: { Attributes -> Attributes }
            : Attributes1 Attribute ',' { $2 . $1 }
            | Attribute ','             { $1 }

Attribute :: { Attributes -> Attributes }
          : rank ':' INT              { addRank (validRank $3) }
          | color ':' Color           { addColor $3 }
          | depends ':' '[' Deps ']'  { addDeps $4 }

Deps :: { [Name] }
     : Deps1 ',' NAME { $3 : $1 }
     | NAME           { [$1] }
     | {- empty -}    { [] }

Deps1 :: { [Name] }
      : Deps1 ',' NAME { $3 : $1 }
      | NAME           { [$1] }

----- End of language definition -----
     
{

validRank n = if n > 0 then Just n else Nothing

parseX11Color :: String -> X11Color
parseX11Color str = case readMaybe str of
  Nothing    -> LightBlue
  Just color -> color

data ParseResult a = Ok a | Failed String
                     deriving Show                     
type LineNumber = Int
type P a = String -> LineNumber -> ParseResult a

getLineNo :: P LineNumber
getLineNo = \s l -> Ok l

thenP :: P a -> (a -> P b) -> P b
m `thenP` k = \s l -> case m s l of
                        Ok a     -> k a s l
                        Failed e -> Failed e
                         
returnP :: a -> P a
returnP a = \s l-> Ok a

failP :: String -> P a
failP err = \s l -> Failed err

catchP :: P a -> (String -> P a) -> P a
catchP m k = \s l -> case m s l of
                        Ok a     -> Ok a
                        Failed e -> k e s l

happyError :: P a
happyError = \s i -> Failed $ "Line "++(show (i::LineNumber))++": Parsing error\n"++(take 10 s)++"..."


data Token =     TName String
               | TOpen
               | TClose 
               | TListOpen 
               | TListClose 
               | TParenOpen 
               | TParenClose 
               | TPrName 
               | TAll 
               | TClusters 
               | TRandom 
               | TOnly 
               | TFromList 
               | TColon
               | TSemiColon
               | TComma
               | TInt Int
               | TRank
               | TColor
               | TDepends
               | TEOF
               deriving Show

----------------------------------
lexer cont s = case s of
                    [] -> cont TEOF []
                    ('\n':s)  ->  \line -> lexer cont s (line + 1)
                    (c:cs)
                          | isSpace c -> lexer cont cs
                          | isDigit c -> lexInt (c:cs)
                          | isAlpha c -> lexVar (c:cs)
                    ('-':('-':cs)) -> lexer cont $ dropWhile ((/=) '\n') cs
                    ('{':('-':cs)) -> consumirBK 0 0 cont cs	
                    ('-':('}':cs)) -> \line -> Failed $ "Line "++(show line)++": Comment wasn't open"
                    ('{':cs) -> cont TOpen cs
                    ('}':cs) -> cont TClose cs
                    ('[':cs) -> cont TListOpen cs
                    (']':cs) -> cont TListClose cs
                    ('(':cs) -> cont TParenOpen cs
                    (')':cs) -> cont TParenClose cs
                    (':':cs) -> cont TColon cs
                    (';':cs) -> cont TSemiColon cs
                    (',':cs) -> cont TComma cs
                    unknown -> \line -> Failed $ 
                     "Line "++(show line)++": Could not parse "++(show $ take 3 unknown)++ "..."
                    where lexVar cs = case span isAlphaNum cs of
                              ("name",rest) -> cont TPrName rest
                              ("all",rest) -> cont TAll rest
                              ("clusters",rest) -> cont TClusters rest
                              ("random",rest) -> cont TRandom rest
                              ("only",rest) -> cont TOnly rest
                              ("fromList",rest) -> cont TFromList rest
                              ("rank",rest)        -> cont TRank rest
                              ("color",rest)       -> cont TColor rest
                              ("depends",rest)     -> cont TDepends rest
                              (name,rest)          -> cont (TName name) rest
                          lexInt cs = case span isDigit cs of
                              (num,rest) -> cont (TInt (read num)) rest
                          consumirBK anidado cl cont s = case s of
                              ('-':('-':cs)) -> consumirBK anidado cl cont $ dropWhile ((/=) '\n') cs
                              ('{':('-':cs)) -> consumirBK (anidado+1) cl cont cs	
                              ('-':('}':cs)) -> case anidado of
                                                  0 -> \line -> lexer cont cs (line+cl)
                                                  _ -> consumirBK (anidado-1) cl cont cs
                              ('\n':cs) -> consumirBK anidado (cl+1) cont cs
                              (_:cs) -> consumirBK anidado cl cont cs     
                                           
depsParse s = parseDeps s 1

}
