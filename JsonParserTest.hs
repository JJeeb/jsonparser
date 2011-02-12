module Main where

import JsonParser
import Test.HUnit
import Text.ParserCombinators.Parsec (ParseError)
import Text.ParserCombinators.Parsec.Error


(>>>) :: String -> Json -> Test
input >>> expected = TestCase $
                     either 
                     (assertFailure  . show) 
                     (assertEqual "Parser Failed" expected) 
                     (parseJsonString  input)


testSring :: Test
testSring = "\"toto\"" >>> JsonString "toto"

testDouble :: Test
testDouble = "12.5" >>> JsonDouble 12.5

testInteger :: Test
testInteger = "123" >>> JsonInteger 123

testTrue :: Test
testTrue = "true" >>> JsonBool True

testFalse :: Test
testFalse = "false" >>> JsonBool False

testNull :: Test
testNull = "null" >>> JsonNull

testArray :: Test
testArray = "[1,2,3]" >>> JsonArray [JsonInteger 1, JsonInteger 2, JsonInteger 3]

testObject = "{\"a\": 11.2, \"b\":null }" >>> JsonObject [("a", JsonDouble 11.2),
                                                          ("b", JsonNull)]


testParser :: Test
testParser =  "{\"a\":[1.0 ,   true   , null, 12, false], \"b\":16.9}" >>>
              JsonObject [ ("a", JsonArray [JsonDouble 1.0, 
                                            JsonBool True, 
                                            JsonNull,
                                            JsonInteger 12, 
                                            JsonBool False]),
                           ("b", JsonDouble 16.9)]
            
testFailure :: Test
testFailure = TestCase $
             let 
                 input = "[1,,3]"
                 parseResult =  parseJsonString  input
             in
               case parseResult of
                 Right actual -> assertFailure "Parser should fail!!"
                 Left _ -> return ()


main :: IO Counts
main = runTestTT $ TestList[testSring, 
                            testDouble, 
                            testInteger,
                            testTrue,
                            testFalse,
                            testNull,
                            testArray,
                            testObject,
                            testParser,
                            testFailure]
       

