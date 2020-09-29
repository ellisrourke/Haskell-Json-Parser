module Main (main) where

import System.IO
import qualified Data.Map as M
import Data.Char

import ABR.Util.Pos
import ABR.Parser
import ABR.Parser.Lexers

data Value = Str String
           | Num Double
          deriving (Show)
--           | Number Double
          

data Object = Object 
   { identifier :: String
   , value :: Value
   } deriving (Show)

makeObject :: String -> Value-> Object
makeObject i v = (Object i v)

symbolL :: Lexer
symbolL = literalL '(' <|> literalL ')' <|> literalL '+' <|>
          literalL '-' <|> literalL '*' <|> literalL '/'

-- string Lexer
myStringL :: Lexer
myStringL =
   literalL '"'
      <&&> (many (satisfyL (/= '"') "") &%> "")
      <&&> literalL '"'
   %> "string"

-- float Lexer
myFloatL :: Lexer
myFloatL =
   fixedL
   <&&> soft (optional (
         (literalL 'e' <|> literalL 'E')
         <&&> soft (optional (
               literalL '-' <|> literalL '+'
              ))
         <&&> nofail' "exponent expected." cardinalL
        ))
   %> "float"

-- Object parser
--objectP :: Parser Object
objectP = 
      literalP "'{'" "{"
  <&> tagP "string"
  <&> literalP "':'" ":"
  <&> valueP
  <&> literalP "'}'" "}"

-- Value parser
valueP :: Parser Value
valueP = 
      tagP "float"
      @> (\(_,n,_) -> Num (read n))
  <|> tagP "string"
      @> (\(_,n,_) -> Str (read n))

objectL:: Lexer
objectL = dropWhite $ nofail $ total $ listL 
   [whitespaceL,symbolL, myFloatL,myStringL]

main :: IO ()
main = do
   putStr "..."
   hFlush stdout
   object <- getLine
   let error :: Pos -> Msg -> IO ()
       error (_,col) msg = do
       putStrLn $ "Error: " ++ msg
       putStrLn object
       let colPrime = if col < 0
             then length object
             else col
       putStrLn $ replicate colPrime ' '
         ++ "^"
       main
   let cps = preLex object
   putStrLn $ "pairs: " ++ show cps
   case objectL cps of
      Error pos msg -> error pos msg
      OK (tlps,_) -> do
         putStrLn $ "Lexemes: " ++ show tlps
         case objectP tlps of
            Error pos msg -> error pos msg
            OK (cmd,_) -> do
               putStrLn $ "Command: "
                  ++ show cmd


{--
let error :: Pos -> Msg -> IO ()
       error (_,col) msg = do
       putStrLn $ "Error: " ++ msg
       putStrLn command
       let colPrime = if col < 0
             then length command
             else col
       putStrL $ replicate colPrime ' '
         ++ "^"
       main
--}



