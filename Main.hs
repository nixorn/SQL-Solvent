{-# LANGUAGE TemplateHaskell #-}

import HFlags

import System.Environment
import System.Exit

import Database.SQL.SQLConverter.Types
import Database.SQL.SQLConverter.Functions
import Database.SQL.SQLConverter.IOFunctions

import TestStuff


defineFlag "o:output" "" "Resulting SQL"

defineFlag "s1:structure_from" "" "File with structure of databases"
defineFlag "s2:structure_to" "" "File with structure of databases"
defineFlag "c:corrs" "" "File with corresponds"


$(return [])								





main :: IO () 
main = do
  _ <- $initHFlags "Some programm. In this stage not worthy to document"
  putStrLn "sdf"
  


data Table = Table [Field]


