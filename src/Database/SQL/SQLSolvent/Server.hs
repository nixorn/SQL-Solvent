{-# LANGUAGE OverloadedStrings #-}
module Database.SQL.SQLSolvent.Server (
  startGui
                                     ) where




import Database.SQL.SQLSolvent.Types



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




