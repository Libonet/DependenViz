{
module Parse where
import DepTree
import Data.GraphViz.Attributes.Colors.X11 (X11Color)

import Data.Maybe
import Data.Char
}

%monad { P } { thenP } { returnP }
%name parseDeps Start

%tokentype { Token }
%lexer {lexer} {TEOF}

%token
    ':'          { TColon }
    ','          { TComma }
    '{'          { TOpen }
    '}'          { TClose }
    '['          { TListOpen }
    ']'          { TListClose }
    int          { TInt $$ }
    rank         { TRank }
    color        { TColor }
    depends      { TDepends }
    NAME         { TName $$ }
    PrNAME       { TPrName }
    
%%

Start :: { Project }
      : PrNAME ':' NAME Nodes         { Pr $3 $4 }
      | Nodes                         { Pr "" $1 }

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
          : rank ':' int              { addRank (validRank $3) }
          | color ':' NAME            { addColor (parseX11Color $3) }
          | depends ':' '[' Deps ']'  { addDeps $4 }

Deps :: { [Name] }
     : Deps ',' NAME                  { $3 : $1 }
     | NAME                           { [$1] }
     | {- empty -}                    { [] }
     
{

validRank n = if n > 0 then Just n else Nothing

parseX11Color :: String -> X11Color
parseX11Color str = read str
  
data ParseResult a = Ok a | Failed String
                     deriving Show                     
type LineNumber = Int
type P a = String -> LineNumber -> ParseResult a

getLineNo :: P LineNumber
getLineNo = \s l -> Ok l

thenP :: P a -> (a -> P b) -> P b
m `thenP` k = \s l-> case m s l of
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
happyError = \ s i -> Failed $ "Line "++(show (i::LineNumber))++": Parsing error\n"++(s)

data Token =     TName String
               | TPrName 
               | TOpen
               | TClose 
               | TListOpen 
               | TListClose 
               | TColon
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
                    -- ('{':('-':cs)) -> consumirBK 0 0 cont cs	
                    -- ('-':('}':cs)) -> \ line -> Failed $ "Línea "++(show line)++": Comentario no abierto"
                    ('{':cs) -> cont TOpen cs
                    ('}':cs) -> cont TClose cs
                    ('[':cs) -> cont TListOpen cs
                    (']':cs) -> cont TListClose cs
                    (':':cs) -> cont TColon cs
                    (',':cs) -> cont TComma cs
                    unknown -> \line -> Failed $ 
                     "Line "++(show line)++": Could not parse "++(show $ take 10 unknown)++ "..."
                    where lexVar cs = case span isAlphaNum cs of
                              ("rank",rest)        -> cont TRank rest
                              ("color",rest)       -> cont TColor rest
                              ("depends",rest)     -> cont TDepends rest
                              ("projectName",rest) -> cont TPrName rest
                              (name,rest)          -> cont (TName name) rest
                          lexInt cs = case span isDigit cs of
                              (num,rest) -> cont (TInt (read num)) rest
                          -- consumirBK anidado cl cont s = case s of
                          --     ('-':('-':cs)) -> consumirBK anidado cl cont $ dropWhile ((/=) '\n') cs
                          --     ('{':('-':cs)) -> consumirBK (anidado+1) cl cont cs	
                          --     ('-':('}':cs)) -> case anidado of
                          --                         0 -> \line -> lexer cont cs (line+cl)
                          --                         _ -> consumirBK (anidado-1) cl cont cs
                          --     ('\n':cs) -> consumirBK anidado (cl+1) cont cs
                          --     (_:cs) -> consumirBK anidado cl cont cs     
                                           
deps_parse s = parseDeps s 1

}
