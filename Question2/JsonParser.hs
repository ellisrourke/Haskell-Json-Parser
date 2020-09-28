module Main (main) where

import System.IO
import qualified Data.Map as M
import Data.Char

import ABR.Util.Pos
import ABR.Parser
import ABR.Parser.Lexers

data Value =  
      String
      | Integer
      | Double
      | Object
    deriving Show
      
type Object = [(String,[Value])]

symbolL :: Lexer
symbolL = literalL '(' <|> literalL ')' <|> literalL '+' <|>
          literalL '-' <|> literalL '*' <|> literalL '/'

 









main :: IO ()
main = print "test"


