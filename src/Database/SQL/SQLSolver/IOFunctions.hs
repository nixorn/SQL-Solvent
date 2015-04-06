module Database.SQL.SQLSolver.IOFunctions (
    tryToOpenFile,
    parseRow
	
) where

import Database.SQL.SQLSolver.Types
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

parseRow :: Parser (String, String, String, String, String, String)
parseRow = do
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
    
parseRawScheme :: Parser [(String, String, String, String, String, String)]

--esrnCsvImport :: IO String -> Maybe Scheme
--esrnCsvImport path = do

