{-# Language OverloadedStrings #-}
module Database.SQL.SQLConverter.Functions (
	
    --reimport
    csvFile
    ,getScheme
    
) where

import Database.SQL.SQLConverter.Types

import Prelude hiding (concat, takeWhile)
import Control.Applicative ((<$>), (<|>), (<*>), (<*), (*>), many)
import Control.Monad (void)
import Data.Attoparsec.Text
import qualified Data.List as L
import qualified Data.Text as T 
import qualified Data.Set as S  

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

    
--------------------------------------------------------------------------------    

atomTable :: [T.Text] -> Table --атомарная "таблица" с  именем и одним полем. Фактически сейчас имеем множество полей
atomTable field  
  | fieldType == "RegularField" = Table tableName (S.fromList [RegularField fieldType dataType])
  | fieldType == "Key"          = Table tableName (S.fromList [Key fieldType dataType ])
  | fieldType == "Relation"     = Table tableName (S.fromList [Relation fieldType dataType relationTable relationField])
  where
    fieldType       = field !! 0
    dataType        = field !! 1
    tableName       = field !! 2
    fieldName       = field !! 3
    relationTable   = field !! 4
    relationField   = field !! 5
    
rollup :: S.Set Table -> S.Set Table
rollup setTab = 
    let tabNames = S.toList $ S.map tName setTab
        dummyTable = Table (T.pack "") (S.fromList [])
        tablesByName :: S.Set Table -> TableName -> S.Set Table
        tablesByName setTab tabName = S.filter (\curTable -> tName curTable == tabName) setTab
        
        rollupFields :: S.Set Table -> Table
        rollupFields =  S.foldl (\ a b -> Table (tName b)  $ S.union  (tBody a) (tBody b)) dummyTable 
    in  S.fromList $ fmap (rollupFields . tablesByName setTab) tabNames

    
 
getScheme :: CSV -> Scheme
getScheme csv = 
    rollup . S.fromList $ fmap atomTable csv 
