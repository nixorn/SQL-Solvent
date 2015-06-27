{-# Language OverloadedStrings #-}
module Database.SQL.SQLSolvent.ServerAPI (
    nodeByTableName
    ,tableNameByNode
    ,findEdgesOnNodes
    ,addNodes
    ,hilightNodes
    ,hilightEdges
    ,unlightEdges
    ,unlightNodes
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
    mkGraph ( L.nub $ labNodes  lc ++ getNodesForAdd gl tn)
                (L.nub $ labEdges lc ++ getEdgesForAdd gl tn)
    where

      getNodesForAdd :: GlbGraph -> [TableName] -> [LNode Table]
      getNodesForAdd graph (t:ts) = 
        let node = nodeByTableName graph t
            surround = node : neighbors graph node --таблица плюс все соседние
              
            inSurround :: [TableId] -> LNode Table -> Bool
            inSurround nds (nd, _) = nd `elem` nds


                               
        in filter (inSurround surround) $ labNodes graph ++ getNodesForAdd graph ts
           
      getNodesForAdd graph (t: []) = 
        let node = nodeByTableName graph t
            surround = node : neighbors graph node --таблица плюс все соседние
              
            inSurround :: [TableId] -> LNode Table -> Bool
            inSurround nds (nd, _) = nd `elem` nds
        in filter (inSurround surround) $ labNodes graph

      getNodesForAdd graph [] = [] 

           
      getEdgesForAdd :: GlbGraph -> [TableName] -> [LEdge RelWIthId]
      getEdgesForAdd graph tbls = filter f $ labEdges graph where
          nodes = fmap (nodeByTableName graph) tbls 
          f (n1, n2, _) = or [n1 `elem` nodes, n2 `elem` nodes]
          

hilightNodes :: LocGraph -> Markers -> [TableId] -> Markers --подсветить ноду и все связи с подсвеченными нодами
hilightNodes _ mrkrs [] = mrkrs
hilightNodes lc (nm, em) tids = 
  let hlnodmarks = (zip tids $ repeat True ) ++ filter (\(tid, _) -> not $ tid `elem` tids) nm --маркера подсветки нод
      hlnodes = fmap (\(id,_) -> id) hlnodmarks          
      filterEdges (from,to,_) = and [to `elem` hlnodes, from `elem` hlnodes] 
      fmapEdges (_,_,(id, RelationInGraph ((_,_), (_,_)))) = (id, True)

      hledgmarks = em ++ (fmap fmapEdges $ filter filterEdges $ L.concat $ fmap ( out lc) tids ++ fmap (inn lc) tids)
  in (hlnodmarks, hledgmarks)


hilightEdges :: LocGraph -> Markers -> [EdgeId] -> Markers --подсветить ребрa, и ноды, которые они связывают
hilightEdges _ mrkrs [] = mrkrs
hilightEdges lc (nm, em) eids = 
    let hledgmarks = (zip eids $ repeat True) ++  filter (\(eid, _) -> not $ eid `elem` eids) em --маркера подсветки ребер
        --забрать ребра, схлопнуть ребра в список айди, нод соединяемых ребрами, удалить дубликаты 
        nodes = L.nub $ L.concat $ fmap (\(n1,n2,_) -> [n1,n2])  $ filter (\(_,_,(id, _)) -> id `elem` eids) $ (labEdges lc)

    in  hilightNodes lc (nm, hledgmarks) nodes


unlightEdges :: LocGraph -> Markers -> [EdgeId] -> Markers  --снять подсветку с ребер, с нод подсветку не снимать
unlightEdges _ mrkrs [] = mrkrs
unlightEdges lc (nm, em) eids =
    let hledgmarks = (zip eids $ repeat False) ++ filter (\(eid, _) -> not $ eid `elem` eids) em --маркера подсветки ребер
        
    in  (nm, hledgmarks )

              
unlightNodes :: LocGraph -> Markers -> [TableId] -> Markers  --снять подсветку с нод и всех их ребер
unlightNodes _ mrkrs [] = mrkrs
unlightNodes lc (nm, em) tids  =
    let hledgmarks = snd
                     $ unlightEdges lc (nm, em)     -- отсветить ноды по id
                     $ fmap (\(_,_,(eid,_)) -> eid) -- забрать id нод
                     $ filter (\(n1, n2, _) -> or [n1 `elem` tids, n2 `elem` tids]) -- которые вяжут какие нибудь таблицы из переданных
                     $ labEdges lc --все ребра локального графа
        hlnodmarks = (zip tids $ repeat False) ++ filter (\(tid, _) -> not $ tid `elem` tids) nm --маркера подсветки нод

    in (hlnodmarks, hledgmarks)


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
