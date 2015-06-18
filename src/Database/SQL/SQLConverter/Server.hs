{-# LANGUAGE OverloadedStrings #-}
module Database.SQL.SQLConverter.Server (
  startGui
                                     ) where




import Database.SQL.SQLConverter.Types



import           Control.Applicative
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server

import qualified Data.Text as T

startGui :: IO ()
startGui = quickHttpServe site

site :: Snap ()
site =
    ifTop (serveFile "./static/index.html") <|>
    dir "static" (serveDirectory "./static")




