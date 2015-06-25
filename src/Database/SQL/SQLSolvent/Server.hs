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
    dir "upload"     (filehandler e_ment )                      <|> -- >> redirect "canvas"
    dir "addtables"  (parseAddResponse e_ment)                  <|>
    dir "canvas"     (renderRequest e_ment)                     <|>
  --dir "changemark" ()                                         <|>
    dir "static"    (serveDirectory "./static")
   
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

renderRequest :: MonadSnap m => MVar GraphEnv -> m ()
renderRequest e_ment = (liftIO $ (do
    e <- takeMVar e_ment
    forkIO $ putMVar e_ment e 
    let lc_graph = localGraph e
        mrkrs    = markers e
        renderNode (id, Table name descr _) = (id, encodeUtf8 name, encodeUtf8 descr)
        renderEdge (from,to, (id, RelationInGraph ((_,fromName), (_,toName)))) = (from,to, (id, (fromName,toName)))
    return $ T.pack $ show (
        fmap renderNode $ labNodes lc_graph,
        fmap renderEdge $ labEdges lc_graph,                                                                                                                 
        mrkrs)))  >>= writeText 

        
        
        
