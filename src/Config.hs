module Config
where

import Database.Persist.Postgresql (ConnectionString)
import Data.ByteString.Char8 (pack)

port :: Int
port = 8080

connString :: ConnectionString
connString = pack "host=postgres port=5432 user=postgres dbname=postgres password=postgres"
