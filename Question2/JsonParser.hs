-- JSON Parser
-- use: JsonParser script

module Main (main) where

import System.Environment
import System.IO
import qualified Data.Map as M
import Data.Char
import Data.Bool


import ABR.Util.Pos
import ABR.Parser
import ABR.Parser.Lexers


data Value = Str String
           | Num Double
           | Arr [Integer]
          deriving (Show)


{--
data Object = Object {identifier :: String , value :: Value}
            deriving (Show)
--}

data Property = Property {identifier:: String, value :: Value}
   deriving(Show)

data Object = Object [Property]
   deriving(Show)

makeProperty :: String -> Value-> Property
makeProperty i v = (Property i v)


symbolL :: Lexer
symbolL = literalL '(' <|> literalL ')' <|> literalL '+' <|>
          literalL '-' <|> literalL '*' <|> literalL '/' <|>
          literalL '{' <|> literalL ':' <|> literalL '}' <|>
          literalL ','

{--object Lexer
objectLex :: Lexer
objectLex = literalL '{'
       <&&> soft (optional (
           (many (satisfyL (/= '}') "") &%> ""))
           )
       <&&> literalL '}'
   %> "object"
--}

--object Lexer
objectLex :: Lexer
objectLex = literalL '{'
       <&&> (many (satisfyL (/= '}') "") &%> "")
       <&&> literalL ':'
       <&&> (many (satisfyL (/= '}') "") &%> "")
       <&&> literalL '}'
   %> "object"

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


-- Array Lexer
arrayL =
   literalL '['
   <&&> soft (optional (
      (many (satisfyL (/= ']') "") &%> "")))
   <&&> literalL ']'
   %> "array"

{--Property parser
--propertyP :: Parser Object
propertyP =
     literalP "'{'" "{"
  &> tagP "string"
  <&> literalP "':'" ":"
  &> valueP
  <& literalP "'}'" "}"
  %> "property"
  --@> (\((_,tag,_),value) -> makeProperty tag value)
--}

--propertyP :: Parser Object
propertyP =
  tagP "string"
  <&> literalP "':'" ":"
  &> valueP
  @> (\((_,tag,_),value) -> makeProperty tag value)

  {--%> "property"--}




-- Value parser
-- valueP :: Parser Value @> (\(_,n,_) -> Str (read n))
valueP =
      tagP "float"
      @> (\(_,n,_) -> Num (read n))
  <|> tagP "string"
      @> (\(_,n,_) -> Str (read n))
  <|> tagP "array"
      @> (\(_,n,_) -> Arr (read n))


propertyL:: Lexer
propertyL = dropWhite $ nofail $ total $ listL
   [whitespaceL,symbolL,myFloatL,myStringL,arrayL]

{--objectP :: Parser property
objectP =
         literalP "'{'"
      &> (many propertyP)
      <& literalP "'}'"
      @> (\(property)) -> []
--}

--objectP :: Parser Program
objectP =
   literalP "'{'" "{"
   &> (many propertyP)
   <& literalP "'}'" "}"
   @> (\(properties) -> [Object properties])


main :: IO()
main = do
   args <- getArgs
   case args of
      [path] -> readInputFile path
      _      -> error "Wrong number of arguments"

run :: IO ()
run = readInputFile "example.txt"


readInputFile :: FilePath -> IO ()
readInputFile path = do
   source <- readFile path
   putStrLn "------- Source Code -------"
   putStrLn source
   let cps = preLex source
   putStrLn "----------- cps -----------"
   print cps
   case propertyL cps of
      Error pos msg -> putStr $ errMsg pos msg source
      OK(tlps,_) -> do
         putStrLn "--------- Lexemes ---------"
         print tlps
         case objectP tlps of
            Error pos msg -> putStr $ errMsg pos msg source
            OK (property,_) -> do
               putStrLn "-------- Object(s) --------"
               print property




































{--
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
