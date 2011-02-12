{-# LANGUAGE QuasiQuotes #-} 

module Main where

import JsonParser (Json (..)) 
import Test.HUnit
import JsonQQ (json)


testQQ :: Test
testQQ = TestCase $ assertEqual "Failure" 
         (JsonObject [ ("a", JsonArray [JsonDouble 1.0, 
                                            JsonBool True, 
                                            JsonNull,
                                            JsonInteger 12, 
                                            JsonBool False]),
                           ("b", JsonDouble 16.9)]) 
         [$json| {"a":[1.0 , true , null, 12, false], "b":16.9} |]




main :: IO Counts
main = runTestTT $ TestList[testQQ]
       