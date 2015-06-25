{-# Language OverloadedStrings #-}
module Database.SQL.SQLSolvent.ServerAPI (
    nodeByTableName
    ,tableNameByNode
    ,findEdgesOnNodes
    ,addNodes
) where

{-
что может быть в реквесте и что должно быть в респонсе?
реквест                                     респонс
массив с именами  таблиц                    массив с именами, номерами таблиц и их окрестностями(глубина =1)
                                            ,связями между ними,  маркерами подсветки таблиц и связей
команды подсветки/отсветки ноды/связи       тот же массив с измененными маркерами
добавление/удаление таблицы(имя)            тот же массив с добавленными/удаленными элементами(и маркерами по необходимости)

Имеем глобальный граф. Потом имеем маленький подграф, с которым юзер работает в данный момент. 
-}

import Database.SQL.SQLSolvent.Types
import Database.SQL.SQLSolvent.NaiveSqlTypes
import Database.SQL.SQLSolvent.Functions

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.List as L
import Data.Graph.Inductive 
import Database.SQL.SQLSolvent.Functions





 




addNodes :: GlbGraph -> LocGraph -> [TableName] -> LocGraph --добавление кучки новых нод и ребер ессесно
addNodes gl lc tn =
    mkGraph (labNodes  lc ++ getNodesForAdd gl tn) (labEdges lc ++ getEdgesForAdd gl tn)
    where

      getNodesForAdd :: GlbGraph -> [TableName] -> [LNode Table]
      getNodesForAdd graph (t:ts) = 
        let node = nodeByTableName graph t
            surround = node : neighbors graph node --таблица плюс все соседние
              
            inSurround :: [TableId] -> LNode Table -> Bool
            inSurround nds (nd, _) = nd `elem` nds
              
        in filter (inSurround surround) $ labNodes graph ++ getNodesForAdd graph ts

      getEdgesForAdd :: GlbGraph -> [TableName] -> [LEdge RelWIthId]
      getEdgesForAdd graph tbls = filter f $ labEdges graph where
          nodes = fmap (nodeByTableName graph) tbls 
          f (n1, n2, _) = or [n1 `elem` nodes, n2 `elem` nodes]
          
 
hilightEdge :: LocGraph -> Markers -> EdgeId -> Markers --подсветить ребро, и ноды, которые оно связывает
hilightEdge = undefined     

hilightNode :: LocGraph -> Markers -> TableId -> Markers --подсветить ноду и все связи с подсвеченными нодами
hilightNode = undefined   

unlightEdge :: LocGraph -> Markers -> EdgeId -> Markers  --снять подсветку с ребра, с нод подсветку не снимать
unlightEdge = undefined    

unlightNode :: LocGraph -> Markers -> TableId -> Markers  --снять подсветку с ноды и всех его ребер
unlightNode = undefined   




--тут мы строим тело респонса

renderRequest :: LocGraph -> Markers -> String
renderRequest lc markers = undefined



nodeByTableName :: Gr Table RelWIthId -> TableName -> Int
nodeByTableName graph tname =
    let labelHasName :: TableName -> LNode Table -> Bool
        labelHasName tname (_, table) = tName table == tname
    in case (L.find (labelHasName tname)  $ labNodes graph) of
        Just (a,_) -> a
        Nothing    -> 0
        
tableNameByNode :: Gr Table RelWIthId -> Node -> String
tableNameByNode graph node = T.unpack 
    . tName 
    $ case (lab graph node) of
        Just a -> a
        Nothing -> dummyTable
        
filterEdgesByNode :: Gr Table RelWIthId -> String -> LEdge RelWIthId -> Bool
filterEdgesByNode graph table (a,b,_) =
        let node = nodeByTableName graph $ T.pack $ table
        in  or [a == node, b == node]


findEdgesOnNodes :: Gr Table RelWIthId -> [String] -> [RelWIthId]
findEdgesOnNodes graph tables = 
    let nodes = fmap ((nodeByTableName graph) . T.pack )  tables
        filterRelations nods (a,b,_) =  and [a `elem` nods, b `elem` nods]
        pprint (_,_, a) = a
    in  fmap pprint $ filter (filterRelations nodes) $ labEdges graph
