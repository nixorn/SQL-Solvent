module Database.SQL.SQLConverter.Functions (
	
    --reimport
    parseRawSchemeEntry,
    parseRawScheme
) where

import Database.SQL.SQLConverter.Types

import Control.Exception
import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import Data.Attoparsec.Combinator


---------
--parsers
---------

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

