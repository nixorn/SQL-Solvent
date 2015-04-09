module Database.SQL.SQLConverter.IOFunctions (
    getFile
	
) where

import Database.SQL.SQLConverter.Functions
import Database.SQL.SQLConverter.Types
import Control.Exception
import Data.Either (rights)

import Data.Attoparsec.Char8
import qualified  Data.ByteString as B 



getFile :: String -> IO B.ByteString
getFile fp = B.readFile fp


        
parseScheme :: String -> IO ()
parseScheme path = do
    file <- getFile path
    let 
        rawScheme :: B.ByteString -> RawScheme
        rawScheme f = case (parseOnly parseRawScheme f) of
            Right  rs -> rs
            Left  s -> [("error","error","error","error","error","error")]
    print $ rawScheme file


