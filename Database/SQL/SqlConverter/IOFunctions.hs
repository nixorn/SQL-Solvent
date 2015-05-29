{-# Language OverloadedStrings #-}
module Database.SQL.SQLConverter.IOFunctions (
    getFile,
    printScheme,
    --parser
    csvFile,
    --attoparsec
    parse, 
    parseOnly
    	
) where

import Database.SQL.SQLConverter.Functions
import Database.SQL.SQLConverter.Types
import Control.Exception

import Data.Attoparsec.Text
import qualified Data.Set as S
import qualified Data.Text as T 

getFile :: FilePath -> IO T.Text
getFile fp = do
    file <- readFile fp
    return $ T.pack file 

        
printScheme :: String -> IO ()
printScheme path = do
    
    file <- getFile path
    let raw = case (parseOnly csvFile file) of
                Right a -> a
                Left _ -> [[]]
      
    print $ getScheme raw

redirectScheme :: String -> IO Scheme
redirectScheme path = do
    file <- getFile path
    let raw = case (parseOnly csvFile file) of
                Right a -> a
                Left _ -> [[]]
      
    return $ getScheme raw
