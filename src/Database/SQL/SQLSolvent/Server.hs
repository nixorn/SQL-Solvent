{-# LANGUAGE OverloadedStrings #-}
module Database.SQL.SQLSolvent.Server (
  startGui
                                     ) where


import Database.SQL.SQLSolvent.Types
import Database.SQL.SQLSolvent.Functions
import Database.SQL.SQLSolvent.IOFunctions
import Database.SQL.SQLSolvent.ServerAPI


import qualified Data.ByteString.Lazy.UTF8 as U
import           Text.Read
import           Control.Applicative 
import           Snap.Core 
import           Snap.Util.FileUploads
import           Snap.Util.FileServe
import           Snap.Http.Server
import           Control.Concurrent
import           Network.CGI (liftIO)

import Data.Graph.Inductive as G
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)


--для отрисовки/хранения
type Gra = Gr Table RelWIthId --граф

--окружение из большого неизменного и маленького постоянно меняющегося графа
data GraphEnv  = GEV { markers    :: Markers --метки подсветки
                    , localGraph  :: Gra     --локальный маленький граф 
                    , globalGraph :: Gra}    --глобальный граф

               
--постройка окружения
buildEMent :: IO (MVar GraphEnv)
buildEMent = newMVar $       
                GEV {markers      = ([],[]) 
                    ,localGraph   = buildEmptyTableGraph
                    ,globalGraph  = buildEmptyTableGraph}

startGui :: IO ()
startGui = do
  e_ment <- buildEMent
  quickHttpServe (site e_ment)


site :: MVar GraphEnv -> Snap ()
site e_ment =
    ifTop            (serveFile "./static/index.html")          <|>
    dir "static"     (serveDirectory "./static")                <|>
    dir "upload"     (filehandler      e_ment )                 <|> -- >> redirect "canvas"
    dir "addtables"  (parseAddResponse e_ment)                  <|>
    dir "canvas"     (renderRequest    e_ment)                  <|>
    dir "unlight"    (handleUnlight    e_ment)                  <|>
    dir "hilight"    (handleHilight    e_ment)                  <|>
    dir "delete"     (handleDelete     e_ment)
--    dir "rendersql"  (renderSql        e_ment)

   
----------получаем файл от юзера
--то, что в итоге тянет файл
filehandler e_ment = handleFileUploads "temp" myDefaultPolicy myPerPartPolicy (buildTabGraSnap e_ment)


maxMb = 10             --файл не более 10
megaByte = 2^(20::Int) --мегабайт

--это общая политика загрузки. общий объем не более стольки
myDefaultPolicy :: UploadPolicy
myDefaultPolicy = setMaximumFormInputSize (maxMb * megaByte) defaultUploadPolicy 

--это пофайловая политика. 
myPerPartPolicy :: PartInfo -> PartUploadPolicy
myPerPartPolicy _ = allowWithMaximumSize (maxMb * megaByte)


--собственно стройка графа из файла
buildTabGraSnap :: MonadSnap m => MVar GraphEnv 
    -> [(PartInfo, Either PolicyViolationException FilePath)] 
    -> m ()
buildTabGraSnap _ ((_,Left exc):_) = do 
    liftIO $ forkIO $ putStrLn $ show exc 
    return ()
buildTabGraSnap e_ment ((_ ,Right filepath):_) = do
  liftIO $ do
    scheme <- redirectScheme filepath
    let graph = buildTableGraph scheme
    putStrLn $ show scheme
    e <- takeMVar e_ment
    putMVar e_ment $ e {globalGraph = graph}
  return ()

--getRequest

--тут мы принимаем данные из GUI и пишем все в окружение
--тут нам расширяют локальный граф массивом табличек через имена
parseAddResponse :: MonadSnap m => MVar GraphEnv  -> m ()
parseAddResponse e_ment  = do
    body <- readRequestBody 100000
    liftIO $ forkIO $ putStrLn $ show body
    case (readMaybe (U.toString body) :: Maybe [String] ) of
        Just tns -> liftIO $ do  
            e <- takeMVar e_ment
            let gl_graph = globalGraph e
                lc_graph = localGraph e
                mrkrs    = markers e
                tabnames = fmap T.pack tns
                new_lc_graph = addNodes gl_graph lc_graph tabnames
            liftIO $ forkIO $ putStrLn $ show new_lc_graph
            putMVar e_ment $ e {localGraph = new_lc_graph}
        Nothing       -> return ()
        
--тут мы строим тело респонса. глобальную переменную не меняем. 

--TODO: доделать нафиг нормально. Либо через инстанс show либо библиотекой, либо заставить интерфейсников
renderRequest :: MonadSnap m => MVar GraphEnv -> m ()
renderRequest e_ment =
    (liftIO $ (do
     e <- takeMVar e_ment
     putMVar e_ment e 
     let lc_graph = localGraph e
         mrkrs    = markers e
         renderNode (id, Table name descr _) = "[" ++ show id ++ "," ++ T.unpack name ++ "," ++ T.unpack descr ++ "]"
         renderEdge (from,to, (id, RelationInGraph ((_,fromName), (_,toName)))) = 
             "[" ++ show from ++ "," ++ show to ++ ",[" ++ show id ++ ",[" ++ T.unpack fromName ++ "," ++ T.unpack toName ++ "]]]"
        
         renderMark ([], []) = "[[],[]]" 
         renderMark ([], edges) = "[[[]]," 
              ++ "[" ++ (foldl1 (\a b -> a ++ "," ++ b) $ fmap (\(a, b) -> "[" ++ show a ++ "," ++ show b ++ "]") edges) ++ "]" ++ "]"
         renderMark (nodes, []) = "[["
              ++ (foldl1 (\a b -> a ++ "," ++ b) $ fmap (\(a, b) -> "[" ++ show a ++ "," ++ show b ++ "]") nodes) ++ "]" ++ "," 
              ++ "[[]]]"
         renderMark (nodes, edges) = "[" 
              ++ "[" ++ (foldl1 (\a b -> a ++ "," ++ b) $ fmap (\(a, b) -> "[" ++ show a ++ "," ++ show b ++ "]") nodes) ++ "]" ++ "," 
              ++ "[" ++ (foldl1 (\a b -> a ++ "," ++ b) $ fmap (\(a, b) -> "[" ++ show a ++ "," ++ show b ++ "]") edges) ++ "]" ++ "]"
        
         render [] = []      
         render list = foldl1 (\a b -> a ++ "," ++ b)  list
        
     return $ T.pack $ "[" ++
               "[" ++ (render $ fmap renderNode $ labNodes lc_graph) ++ "]" ++ "," ++
               "[" ++ (render $ fmap renderEdge $ labEdges lc_graph) ++ "]" ++ "," ++ 
               "[" ++ renderMark mrkrs ++ "]" 
                ++ "]"))  >>= writeText 

handleHilight :: MonadSnap m => MVar GraphEnv -> m ()
handleHilight e_ment =  do
    body <- readRequestBody 100000
    case (readMaybe (U.toString body) :: Maybe [[Int]]  ) of --на самом деле тут должно быть хотябы ([Int], [Int]), но мы json+хардкодинг.
      Just chngmark -> 
          case (length chngmark == 2 ) of
            True -> (liftIO $ do
                       e <- takeMVar e_ment
                       let lc = localGraph e
                           mrkrs = markers e
                           nodemarkchg = chngmark !! 0
                           edgemarkchg = chngmark !! 1 
                       putMVar e_ment $  e {markers = hilightEdges lc  (hilightNodes lc mrkrs nodemarkchg) edgemarkchg}) >> return ()
                          
            False -> return ()
      Nothing -> return ()


handleDelete :: MonadSnap m => MVar GraphEnv -> m ()
handleDelete e_ment =  do
    body <- readRequestBody 100000
    case (readMaybe (U.toString body) :: Maybe [[Int]]  ) of --на самом деле тут должно быть хотябы ([Int], [Int]), но мы json+хардкодинг.
      Just todel -> 
          case (length todel == 2 ) of
            True -> (liftIO $ do
                       e <- takeMVar e_ment
                       let lc = localGraph e
                           mrkrs = markers e
                           nodedel = todel !! 0
                           edgedel = todel !! 1
                           
                       putMVar e_ment $  e {localGraph = delEdges' (delNodes' lc nodedel) edgedel}
                                           --,  markers =
                                           ) >> return ()
                          
            False -> return ()
      Nothing -> return ()
    



handleUnlight :: MonadSnap m => MVar GraphEnv -> m ()
handleUnlight e_ment =  do
    body <- readRequestBody 100000
    case (readMaybe (U.toString body) :: Maybe [[Int]]  ) of --на самом деле тут должно быть хотябы ([Int], [Int]), но мы json+хардкодинг.
      Just chngmark -> 
          case (length chngmark == 2 ) of
            True -> (liftIO $ do
                       e <- takeMVar e_ment
                       let lc = localGraph e
                           mrkrs = markers e
                           nodemarkchg = chngmark !! 0
                           edgemarkchg = chngmark !! 1 
                       putMVar e_ment $  e {markers = unlightEdges lc  (unlightNodes lc mrkrs nodemarkchg) edgemarkchg}) >> return ()
                          
            False -> return ()
      Nothing -> return ()
    



