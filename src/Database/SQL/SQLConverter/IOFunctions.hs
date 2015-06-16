{-# Language OverloadedStrings #-}
module Database.SQL.SQLConverter.IOFunctions (

    getSchemeGraph
    ,tnames
    ,redirectScheme
    ,getFile
    
) where

import Database.SQL.SQLConverter.Functions
import Database.SQL.SQLConverter.Types
import Database.SQL.SQLConverter.Gui
import Control.Exception


import Data.Graph.Inductive.Graph 
import Data.Graph.Inductive


import Data.Attoparsec.Text
import qualified Data.Set as S
import qualified Data.Text as T 


--дляконсольный отладочный стафф 
csvpath = "example.csv"
   
getSchemeGraph :: IO (Gr Table RelationInGraph)
getSchemeGraph = do
    scheme <- redirectScheme csvpath
    return $ buildTableGraph scheme
  
tnames :: Gr Table RelationInGraph -> [Node] -> [TableName]
tnames graph nodes  = fmap (\node -> case (lab graph node) of
                                            Just a -> tName a 
                                            Nothing -> T.pack "") nodes
                                            

    
--IO

getFile :: FilePath -> IO T.Text
getFile fp = do
    file <- readFile fp
    return $ T.pack file 


redirectScheme :: String -> IO Scheme
redirectScheme path = do
    file <- getFile path
    let raw = case (parseOnly csvFile file) of
                Right a -> a
                Left _ -> [[]]
      
    return $ getScheme raw


