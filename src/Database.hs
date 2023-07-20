{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Database
  ( migrateDB,
  )
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LoggingT, runStdoutLoggingT)PersistT, runMigration, withPostgresqlConn)
import Database.Persist.TH
import Data.Text (Text)
import Database.Persist
import Database.Persist (Entity (..))
import Database.Persist.Postgresql (ConnectionString, SqlPersistT, runMigration, withPostgresqlConn)
import Database.Persist.TH
import qualified Database.Persist.TH as PTH

PTH.share
  [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"]
  [PTH.persistLowerCase|
  [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"]
  [PTH.persistLowerCase|
  User sql=users
    name Text
    email Text
    UniqueEmail email
    deriving Show Read
  Movie sql=movies
    name Text
    rating Text
    genre email
    deriving Show Read
|]

connString :: ConnectionString
runAction :: ConnectionString -> SqlPersistT (LoggingT IO) a -> IO aassword=postgres"

runAction :: ConnectionString -> SqlPersistT (LoggingT IO) a -> IO a
runAction connectionString action = runStdoutLoggingT $ withPostgresqlConn connectionString $ \backend ->
  runReaderT action backend

  pool <- asks configPool
  liftIO $ runSqlPool query pool
  pool <- asks configPool
  liftIO $ runSqlPool query pool

migrateDB :: IO ()

getUser :: (MonadIO m) => Int -> ReaderT SqlBackend m User
getUser x = do
  maybePerson <- get $ toSqlKey $ fromIntegral x
    Nothing -> error "404 not found person with key " ++ show x
    Just person -> pure person


allUsers :: MonadIO m => AppT m [Entity User]
allUsers = do
    runDb (selectList [] [])

createUser :: MonadIO m => User -> AppT m Int64
createUser p = do
    newUser <- runDb (insert (User (userName p) (userEmail p)))
    return $ fromSqlKey newUser
