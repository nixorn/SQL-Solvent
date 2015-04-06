module Database.SQL.SQLConverter.IOFunctions (
    tryToOpenFile,
    parseRawSchemeEntry,
    parseRawScheme
	
) where

import Database.SQL.SQLConverter.Types
import Control.Exception
import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import Data.Attoparsec.Combinator
import Codec.Binary.UTF8.String  as U
import qualified Data.ByteString as B


tryToOpenFile :: FilePath -> IO String
tryToOpenFile path =
    handle possibleErrors (readFile path) 
    where
        possibleErrors :: IOException -> IO String
        possibleErrors error = return "Aaaaa!!! Please check file."

parseRawSchemeEntry :: Parser RawSchemeEntry
parseRawSchemeEntry = do
    fieldType <- many anyChar
    char ';'
    dataType <- many anyChar
    char ';'
    tableName <- many anyChar
    char ';'
    fieldName <- many anyChar
    char ';'
    linkTable <- many anyChar
    char ';'
    linkField <- many anyChar
    return (fieldType, dataType, tableName, fieldName, linkTable, linkField)
    
parseRawScheme :: Parser RawScheme
parseRawScheme = many $ parseRawSchemeEntry <* endOfLine
    

--esrnCsvImport :: IO String -> Maybe Scheme
--esrnCsvImport path = do

