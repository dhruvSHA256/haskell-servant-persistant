{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Database
where


import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger
import Database.Persist.TH
import qualified Database.Persist.TH as PTH
import Data.Text (Text)
import Data.Int (Int64)
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.Sql (selectList)

PTH.share [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"] [PTH.persistLowerCase|
  User sql=users
    name Text
    email Text
    UniqueEmail email
    deriving Show Read
|]

connString :: ConnectionString
connString = "host=postgres port=5432 user=postgres dbname=postgres password=postgres"

runAction :: ConnectionString -> SqlPersistT (LoggingT IO) a ->  IO a
runAction connectionString action = runStdoutLoggingT $ withPostgresqlConn connectionString $ \backend ->
  runReaderT action backend

migrateDB :: IO ()
migrateDB = runAction connString (runMigration migrateAll)

getUser :: Int64 -> IO (Maybe User)
getUser uid = runAction connString (get (toSqlKey uid))

createUser :: User -> IO Int64
createUser user = fromSqlKey <$> runAction connString (insert user)

deleteUser :: Int64 -> IO ()
deleteUser uid = runAction connString (delete userKey)
  where
    userKey :: Key User
    userKey = toSqlKey uid
