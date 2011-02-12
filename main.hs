module Main where

import  System.Environment (getArgs)
import JsonParser  (jsonFile)
import Text.ParserCombinators.Parsec (parse, sourceName, sourceLine, errorPos) 

main :: IO ()
main = do arguments <- getArgs
          let cmd = arguments !! 0
          case cmd of
            "check" -> check $ arguments !! 1
            "pprint" -> pprint $ arguments !! 1

check :: FilePath -> IO() 
check filename = 
    do input <- readFile filename
       case parse jsonFile filename input of
         Right _ ->  return ()
         Left failure -> do 
           let pos = errorPos failure
           putStrLn $ "  File \""  ++ (sourceName pos) ++ "\", line " ++ (show . sourceLine)  pos
           print failure


pprint :: FilePath -> IO()
pprint filename = do 
  do input <- readFile filename
     case parse jsonFile filename input of
       Right jsonValue -> print jsonValue
       Left failure -> do 
                    let pos = errorPos failure
                    putStrLn $ "  File \""  ++ (sourceName pos) ++ "\", line " ++ (show . sourceLine)  pos
                    print failure
  