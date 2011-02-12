{-# LANGUAGE DeriveDataTypeable #-}


module JsonParser where

import Control.Monad (liftM)
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as Token
import Text.ParserCombinators.Parsec.Language
import Data.List (intercalate)
import Data.Data

data Json = JsonString String
  | JsonDouble Double
  | JsonInteger Integer
  | JsonBool Bool
  | JsonNull
  | JsonArray [Json]
  | JsonObject [(String, Json)]
  deriving (Eq, Typeable, Data)


instance Show Json where
    show (JsonString s) = show s
    show (JsonDouble d) = show d
    show (JsonInteger i) = show i
    show (JsonBool b) = case b of
                          True -> "true"
                          False -> "false"
    show JsonNull = "null"
    show (JsonArray arr) = "[" ++ 
                           intercalate ", "  (map show arr) 
                           ++ "]"
    show (JsonObject assocList) = "{" ++ 
                                  intercalate ", " (map (\(a, b) -> (show a) ++ ": " ++ (show b)) assocList)
                                  ++ "}"

          
lexer = Token.makeTokenParser emptyDef {commentStart = "/*",
                                        commentEnd = "*/",
                                        commentLine = "//"}

symbol          = Token.symbol        lexer
stringLiteral   = Token.stringLiteral lexer
float           = Token.float         lexer
integer         = Token.integer       lexer
squares         = Token.squares       lexer
braces          = Token.braces        lexer
commaSep        = Token.commaSep      lexer

($>) :: (Monad m) => m a1 -> (a1 -> r) -> m r
a $> b = liftM b a

json :: Parser Json
json = choice [ stringLiteral              $> JsonString, 
                try float                  $> JsonDouble,
                integer                    $> JsonInteger,
                (true <|> false)           $> JsonBool,
                symbol "null"              $> const JsonNull,
                (squares . commaSep) json  $> JsonArray,
                (braces . commaSep)  assoc $> JsonObject ]
         where
           true  = symbol "true"  $> const True 
           false = symbol "false" $> const False

           assoc = do key <- stringLiteral
                      symbol ":"
                      val <- json
                      return (key, val)


jsonFile :: Parser Json
jsonFile = do value <- json
              eof
              return value
                                
parseJsonString :: String -> Either ParseError Json 
parseJsonString input = parse jsonFile "" input
