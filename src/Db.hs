{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}

import           Control.Monad.IO.Class  (liftIO)
import           Database.Persist
import           Database.Persist.TH
import qualified Database.Persist.TH as PTH
import Database.Persist (Entity(..))
import Data.Text (Text)
import Control.Monad.Logger (runStdoutLoggingT, LoggingT)
import Database.Persist.Postgresql (ConnectionString, withPostgresqlConn, SqlPersistT, runMigration)

PTH.share [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"] [PTH.persistLowerCase|
  User sql=users
    name Text
    email Text
    age Int
    occupation Text
    UniqueEmail email
    deriving Show Read
|]

connString :: ConnectionString
connString = "host=127.0.0.1 port=5432 user=postgres dbname=postgres password=postgres"

runAction :: ConnectionString -> SqlPersistT (LoggingT IO) a ->  IO a
runAction connectionString action = runStdoutLoggingT $ withPostgresqlConn connectionString $ \backend ->
  runReaderT action backend

migrateDB :: IO ()
migrateDB = runAction connString (runMigration migrateAll)
