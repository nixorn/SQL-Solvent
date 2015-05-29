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
import Data.Graph.Inductive as G

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
--получение схемы
atomTable :: [T.Text] -> Table --атомарная "таблица" с  именем и одним полем. Фактически сейчас имеем множество полей
atomTable field  
  | fieldType == "RegularField" = Table tableName (S.fromList [RegularField fieldName dataType])
  | fieldType == "Key"          = Table tableName (S.fromList [Key fieldName dataType ])
  | fieldType == "Relation"     = Table tableName (S.fromList [Relation fieldName dataType relationTable relationField])
  | otherwise = Table (T.pack "") (S.fromList [])
  where
    fieldType       = field !! 0
    dataType        = field !! 1
    tableName       = field !! 2
    fieldName       = field !! 3
    relationTable   = field !! 4
    relationField   = field !! 5
    
rollup :: S.Set Table -> S.Set Table --свертка атомарных таблиц в одну
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

--------------------------------------------------------------------------------    
--графы

buildTableGraph :: G.Graph gr => Scheme -> gr
buildTableGraph scheme = do
    let nodes = zip [1..(S.size scheme)] (S.toList scheme)
        
        isTargetNode :: TableName -> (Int, Table) -> Bool
        isTargetNode tn (_, targt) = tn == tName targt
        
        searchTargetNode :: TableName -> [(Int, Table)] -> Int
        searchTargetNode tname nodes = fst $ head $ filter (isTargetNode  tname) nodes 
        
        getEdge :: [(Int, Table)] -> (Int, Table) -> Field -> LEdge 
        getEdge nodes (node, tbl1) (Relation  fnm dt tblnm2 fnm2) = (node, --исходная нода
            searchTargetNode nodes tblnm2, --целевая нода
            (((tName tbl1), fnm), (tblnm2, fnm2)) --((ссылающаяся таблица, ссылающееся поле),(целевая таблица, целевое поле))
        
        getNodeEdges  :: [LNode] -> LNode -> [LEdge]
        getNodeEdges nodes (node, tbl) = S.toList $ S.map (getEdge nodes (node, tbl)) $ tBody tbl
        
        getAllEdges :: [LNode] -> [LEdge]
        getAllEdges nodes = concat $ fmap (getNodeEdges nodes)  nodes
        
    in mkGraph nodes $ getAllEdges nodes
            

            
            
         
        