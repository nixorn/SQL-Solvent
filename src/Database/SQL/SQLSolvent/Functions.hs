{-# Language OverloadedStrings #-}
module Database.SQL.SQLSolvent.Functions (
	
 
    csvFile
    ,getScheme
    ,buildTableGraph
    ,buildTableGraph'
    ,buildEmptyTableGraph

) where

import Database.SQL.SQLSolvent.Types


import Prelude hiding (concat, takeWhile)
import Control.Applicative ((<$>), (<|>), (<*>), (<*), (*>), many)
import Control.Monad (void)
import Data.Maybe
import Data.Attoparsec.Text
import qualified Data.List as L
import qualified Data.Text as T 
import qualified Data.Set as S  
import Data.Graph.Inductive as G
import Data.Graph.Inductive.Query.BFS(lbft, bft)



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
  | fieldType == "Regular" = Table tableName tableDescr (S.fromList [Regular fieldName dataType fieldDescr ])
  | fieldType == "Key"          = Table tableName tableDescr (S.fromList [Key fieldName dataType fieldDescr])
  | fieldType == "Relation"     = Table tableName tableDescr (S.fromList [Relation fieldName dataType relationTable relationField fieldDescr])
  | otherwise = dummyTable
  where
    fieldType       = field !! 0
    dataType        = field !! 1
    tableName       = field !! 2
    fieldName       = field !! 3
    relationTable   = field !! 4
    relationField   = field !! 5
    tableDescr      = field !! 6
    fieldDescr      = field !! 7
    
rollup :: S.Set Table -> S.Set Table --свертка атомарных таблиц в одну
rollup setTab = 
    let tabNames = S.toList $ S.map tName setTab
        
        tablesByName :: S.Set Table -> TableName -> S.Set Table
        tablesByName setTab tabName = S.filter (\curTable -> tName curTable == tabName) setTab
        
        rollupFields :: S.Set Table -> Table
        rollupFields =  S.foldl (\ a b -> Table (tName b) (description b)  $ S.union  (tBody a) (tBody b)) dummyTable 
    in  S.fromList $ fmap (rollupFields . tablesByName setTab) tabNames

    
 
getScheme :: CSV -> Scheme
getScheme csv = 
    rollup . S.fromList $ fmap atomTable csv 

--------------------------------------------------------------------------------    
--графы
--схема базы данных = граф
--таблица = нода Node или LNode, что эквивалентно
--реляционная связь = ребро - Edge или LEdge, что эквивалентно
--путь связей = путь графа

buildEmptyTableGraph :: Gr Table RelWIthId
buildEmptyTableGraph = G.empty --tempty  ((0, 0, (0,RelationInGraph ((T.empty, T.empty),(T.empty, T.empty)))))

buildTableGraph ::  Scheme -> Gr Table RelWIthId
buildTableGraph scheme =
    let nodes = zip [1..(S.size scheme)] (S.toList scheme) ++ dummyNode --таблицы, разобранные в ноды
        
        isTargetNode :: TableName -> LNode Table -> Bool
        isTargetNode tn (_, targt) = tn == tName targt
                       
        searchTargetNode :: TableName -> [LNode Table] -> Int
        searchTargetNode tname nodes = fst $ head $ filter (isTargetNode  tname) nodes 
            ++ dummyNode 

        getEdge :: [LNode Table] -> LNode Table -> Field -> Maybe (LEdge RelationInGraph)
        getEdge _ _ (Key      _ _ _) = Nothing
        getEdge _ _ (Regular  _ _ _) = Nothing
        getEdge nodes (node, tbl1) (Relation  fnm dt tblnm2 fnm2 des) = Just  (node, 
            searchTargetNode tblnm2 nodes, --целевая нода
            RelationInGraph ((tName tbl1, fnm), (tblnm2, fnm2))) --((ссылающаяся таблица, ссылающееся поле),(целевая таблица, целевое поле))
       
        getNodeEdges  :: [LNode Table] -> LNode Table -> [LEdge RelationInGraph]
        getNodeEdges nodes (node, tbl) = catMaybes $ S.toList $ S.map (getEdge nodes (node, tbl)) $ tBody tbl
        
        getAllEdges :: [LNode Table] -> [LEdge RelationInGraph]
        getAllEdges nodes = L.concat $ fmap (getNodeEdges nodes)  nodes

        packEdges :: [LEdge RelationInGraph] -> [Int] -> [LEdge RelWIthId]
        packEdges []  (i:intes) =  []
        packEdges ((node1, node2, des):[])  (i:intes) =  [(node1, node2, (i, des))]
        packEdges ((node1, node2, des):rs)  (i:intes) =  (node1, node2, (i, des)): (packEdges rs intes)

                                                         
    in  mkGraph nodes $ packEdges (getAllEdges nodes) [1..]

buildTableGraph' a  = undir $ buildTableGraph a  --ненаправленный
    
dummyNode = [(0, dummyTable)]--пустая таблица, чтобы собирать все битые ссылки, чтобы не падали чистые функции, ребра не уходили в пустоту и все такое


