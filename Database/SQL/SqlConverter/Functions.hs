{-# Language OverloadedStrings #-}
module Database.SQL.SQLConverter.Functions (
	
 
    csvFile
    ,getScheme
    ,buildTableGraph
    ,buildTableGraph'
    ,pathArea
    ,edgesArea
    ,nodeByTableName
    ,tableNameByNode
) where

import Database.SQL.SQLConverter.Types
import Database.SQL.SQLConverter.NaiveSqlTypes

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

--отладочный стафф
nodeByTableName :: Gr Table RelationInGraph -> TableName -> Int
nodeByTableName graph tname =
    let labelHasName :: TableName -> LNode Table -> Bool
        labelHasName tname (_, table) = tName table == tname
    in case (L.find (labelHasName tname)  $ labNodes graph) of
        Just (a,_) -> a
        Nothing    -> 0
        
tableNameByNode :: Gr Table RelationInGraph -> Node -> String
tableNameByNode graph node = T.unpack 
    . tName 
    $ case (lab graph node) of
        Just a -> a
        Nothing -> dummyTable


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
  | fieldType == "Key"          = Table tableName (S.fromList [Key fieldName dataType fieldDescr])
  | fieldType == "Relation"     = Table tableName (S.fromList [Relation fieldName dataType relationTable relationField fieldDescr])
  | otherwise = Table (T.pack "") (S.fromList [])
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
        rollupFields =  S.foldl (\ a b -> Table (tName b)  $ S.union  (tBody a) (tBody b)) dummyTable 
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



buildTableGraph ::  Scheme -> Gr Table RelationInGraph
buildTableGraph scheme =
    let nodes = zip [1..(S.size scheme)] (S.toList scheme) ++ dummyNode --таблицы, разобранные в ноды
        
        isTargetNode :: TableName -> LNode Table -> Bool
        isTargetNode tn (_, targt) = tn == tName targt
                       
        searchTargetNode :: TableName -> [LNode Table] -> Int
        searchTargetNode tname nodes = fst $ head $ filter (isTargetNode  tname) nodes 
            ++ dummyNode 

        getEdge :: [LNode Table] -> LNode Table -> Field -> Maybe (LEdge RelationInGraph)
        getEdge _ _ (Key      _ _) = Nothing
        getEdge _ _ (Regular  _ _) = Nothing
        getEdge nodes (node, tbl1) (Relation  fnm dt tblnm2 fnm2) = Just  (node, 
            searchTargetNode tblnm2 nodes, --целевая нода
            RelationInGraph ((tName tbl1, fnm), (tblnm2, fnm2))) --((ссылающаяся таблица, ссылающееся поле),(целевая таблица, целевое поле))
       
        getNodeEdges  :: [LNode Table] -> LNode Table -> [LEdge RelationInGraph]
        getNodeEdges nodes (node, tbl) = catMaybes $ S.toList $ S.map (getEdge nodes (node, tbl)) $ tBody tbl
        
        getAllEdges :: [LNode Table] -> [LEdge RelationInGraph]
        getAllEdges nodes = L.concat $ fmap (getNodeEdges nodes)  nodes
        
    in  mkGraph nodes $ getAllEdges nodes


buildTableGraph' a  = undir $ buildTableGraph a  --ненаправленный
    

dummyNode = [(0, Table (T.pack "NULL") S.empty)]--пустая таблица, чтобы собирать все битые ссылки, чтобы не падали чистые функции, ребра не уходили в пустоту и все такое

--Вычленение интересующего множества связей-таблиц из схемы

--множество таблиц, в которых пролегает разрешение связей. интересуют только пути связей, которые заканчиваются на других  таблицах подграфа
pathArea :: [String] -> Gr Table RelationInGraph -> [[Node]]
pathArea l graph = 
    let leafs = fmap ((nodeByTableName graph) . T.pack) l
        bft1 a b = bft b a
        validPathes :: Gr Table RelationInGraph -> [Node] -> [[[Node]]] -> [[Node]]
        validPathes graph leafs pathess = 
            S.toList 
            . S.fromList 
            . L.concat $ fmap (\pathes ->  
                       filter (\path -> and 
                            [elem (head path) leafs, elem (last path) leafs]) pathes) pathess
    in  validPathes graph leafs 
        $ fmap (bft1 graph) leafs
    
--множество связей между этими узлами, которые заканчиваются в том же множестве узлов

edgesArea :: Gr Table RelationInGraph -> [Node] -> [Edge]
edgesArea graph nodes = 
    let 
        filterEdges :: Gr Table RelationInGraph -> [Node] -> LEdge a -> Bool
        filterEdges graph nodes (n1, n2, _) = and [n1 `elem` nodes, n1 `elem` nodes]
        
        mapEdges :: LEdge a -> Edge
        mapEdges (n1, n2, _) = (n1,n2) 
    in fmap mapEdges $ filter (filterEdges graph nodes ) $ S.toList . S.fromList . L.concat $ fmap (inn graph) nodes ++ fmap (out graph) nodes
    
