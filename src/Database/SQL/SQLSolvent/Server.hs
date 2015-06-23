{-# LANGUAGE OverloadedStrings #-}
module Database.SQL.SQLSolvent.Server (
  startGui
                                     ) where


import Database.SQL.SQLSolvent.Types
import Database.SQL.SQLSolvent.Functions
import Database.SQL.SQLSolvent.IOFunctions
import Database.SQL.SQLSolvent.ServerAPI


import           Control.Applicative 
import           Snap.Core 
import           Snap.Util.FileUploads
import           Snap.Util.FileServe
import           Snap.Http.Server
import           Control.Concurrent

import Data.Graph.Inductive as G
import qualified Data.Text as T


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
    ifTop (serveFile "./static/index.html") <|>
    dir "upload" (filehandler e_ment)   <|> 
    dir "static" (serveDirectory "./static")
   
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
buildTabGraSnap _ ((_,Left _):_) = return ()
buildTabGraSnap e_ment ((_ ,Right filepath):_) = do
  forkIO $ do
    scheme <- redirectScheme filepath
    let graph = buildTableGraph scheme
    e <- takeMVar e_ment
    putMVar e_ment $ e {globalGraph = graph}
  return ()


--то, что g

renderGraph :: Either String (Gr Table RelWIthId) -> Response
renderGraph = undefined

