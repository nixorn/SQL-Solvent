{-# Language OverloadedStrings #-}
module Database.SQL.SQLConverter.Functions (
	
    --reimport
    csvFile
    
) where

import Database.SQL.SQLConverter.Types


import Prelude hiding (concat, takeWhile)
import Control.Applicative ((<$>), (<|>), (<*>), (<*), (*>), many)
import Control.Monad (void)
import Data.Attoparsec.Text
import qualified Data.Text as T (Text, concat, cons, append)
import qualified Data.Map as Map  




--parsers. stolen from
---https://github.com/robinbb/attoparsec-csv/blob/master/Text/ParseCSV.hs 
---------


type CSV = [[T.Text]]

lineEnd :: Parser ()
lineEnd =
    void (char '\n') <|> void (string "\r\n") <|> void (char '\r')
    <?> "end of line"
    
unquotedField :: Parser T.Text
unquotedField =
    takeWhile (`notElem` ";\n\r\"")
    <?> "unquoted field"
    
insideQuotes :: Parser T.Text
insideQuotes =
    T.append <$> takeWhile (/= '"')
        <*> (T.concat <$> many (T.cons <$> dquotes <*> insideQuotes))
    <?> "inside of double quotes"
    where
        dquotes =
            string "\"\"" >> return '"'
            <?> "paired double quotes"
            
quotedField :: Parser T.Text
quotedField =
    char '"' *> insideQuotes <* char '"'
    <?> "quoted field"
    
field :: Parser T.Text
field =
    quotedField <|> unquotedField
    <?> "field"
    
record :: Parser [T.Text]
record =
    field `sepBy1` char ';'
    <?> "record"
    
csvFile :: Parser CSV
csvFile =
    (:) <$> record
    <*> manyTill (lineEnd *> record)
                (endOfInput <|> lineEnd *> endOfInput)
    <?> "file"
 --
getScheme :: CSV -> Scheme
getScheme csv = 
    Scheme $ foldl (\raw = Map.fromList ) $ fmap rollup csv where
        rollup :: [String] -> Table
        rollup field  
          | fieldType == "RegularField" = Table tableName [RegularField fieldType dataType ]
          | fieldType == "Key" = Table tableName [Key fieldType dataType ]
          | fieldType == "Relation" = Table tableName [RegularField fieldType dataType ] 
          where
            fieldType = field !! 0
            dataType = field !! 1
            tableName = field !! 2
            fieldName = field !! 3
            relationTable = field !! 4
            relationField = field !! 5
              
          
 

--[["RegularField;varchar;WS_ULRIC_LOG;A_STREETTYPE;NULL;NULL"],["RegularField;datetime;WS_ULRIC_LOG;A_TIMESTAMP;NULL;NULL"],["RegularField;varchar;WS_ULRIC_LOG;GUID;NULL;NULL"],["RegularField;varchar;WS_ULRICSETTINGS;A_CODE_ORG;NULL;NULL"],["Relation;int;WS_ULRICSETTINGS;A_NAME_AMOUNT;SPR_HSC_TYPES;A_ID"],["Relation;int;WS_ULRICSETTINGS;A_RECEIPT_TYPE;SPR_RECEIPT_TYPE;A_OUID"],["RegularField;varchar;WS_ULRICSETTINGS;A_RIC_CODE;NULL;NULL"],["RegularField;varchar;WS_ULRICSETTINGS;A_USER;NULL;NULL"]]


--Scheme [Table []]
