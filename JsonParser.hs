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
    show (JsonBool True) = "true"
    show (JsonBool False) = "false"
    show JsonNull = "null"
    show (JsonArray arr) = "[" ++ intercalate ", "  (map show arr) ++ "]"
    show (JsonObject assocList) = "{" ++ intercalate ", " (map (\(a, b) -> (show a) ++ ": " ++ (show b)) assocList) ++ "}"

          
lexer = Token.makeTokenParser emptyDef 

symbol          = Token.symbol        lexer
stringLiteral   = Token.stringLiteral lexer
float           = Token.float         lexer
integer         = Token.integer       lexer
squares         = Token.squares       lexer
braces          = Token.braces        lexer
commaSep        = Token.commaSep      lexer


json :: Parser Json
json = choice [ stringLiteral              >>= (return . JsonString), 
                try float                  >>= (return . JsonDouble),
                integer                    >>= (return . JsonInteger),
                (true <|> false)           >>= (return . JsonBool),
                symbol "null"              >>= (return . const JsonNull),
                (squares . commaSep) json  >>= (return . JsonArray),
                (braces . commaSep)  assoc >>= (return . JsonObject) ]
         where
           true  = symbol "true"  >>= (return . const True)
           false = symbol "false" >>= (return . const False)

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
