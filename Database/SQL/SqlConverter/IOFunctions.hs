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
import qualified Data.Text as T (Text, pack)

getFile :: String -> IO T.Text
getFile fp = do
    file <- readFile fp
    return $ T.pack file 

--eax :: String -> Int -> Vector -> String        
printScheme :: String -> IO ()
printScheme path = do
    putStrLn ""
    {-
    file <- getFile path
    let 
        rawScheme :: B.ByteString -> RawScheme
        rawScheme f = case (parseOnly parseRawScheme f) of
            Right  rs ->  rs
            Left  s -> RawScheme [RawSchemeEntry ("error","error","error","error","error","error")]
    print $ rawScheme file-}
