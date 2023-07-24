{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
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
{-# LANGUAGE TypeApplications #-}
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
import Database.Persist.Sql
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON, parseJSON, (.:), withObject)
import Config

PTH.share [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"] [PTH.persistLowerCase|
  User sql=users
    name String
    email String
    password String
    token String Maybe
    UniqueEmail email
    deriving Show Read Generic
  Movie sql=movies
    name String
    rating Double Maybe
    genre String Maybe
    user UserId Maybe
    deriving Show Read Generic
|]


instance FromJSON Movie where
  parseJSON = withObject "Movie" $ \obj ->
    Movie
      <$> obj .: "name"
      <*> obj .: "rating"
      <*> obj .: "genre"
      <*> obj .: "user"

instance ToJSON Movie

runAction :: ConnectionString -> SqlPersistT (LoggingT IO) a ->  IO a
runAction connectionString action = runStdoutLoggingT $ withPostgresqlConn connectionString $ \backend ->
  runReaderT action backend

migrateDB :: IO ()
migrateDB = runAction connString (runMigration migrateAll)

createUser :: User -> IO Int64
createUser user = fromSqlKey <$> runAction connString (insert user)

getUser :: Int64 -> IO (Maybe User)
getUser uid = runAction connString (get userKey)
  where
    userKey :: Key User
    userKey = toSqlKey uid

getAllUser :: IO [Entity User]
getAllUser = runAction connString (selectList [] [])

updateUser :: Int64 -> User -> IO ()
updateUser uid updatedUser = runAction connString $ do
    maybeUser <- get userKey
    case maybeUser of
        Nothing -> liftIO $ putStrLn "User not found."
        Just _ -> replace (toSqlKey uid) updatedUser
    where
      userKey :: Key User
      userKey = toSqlKey uid

deleteUser :: Int64 -> IO ()
deleteUser uid = runAction connString (delete userKey)
  where
    userKey :: Key User
    userKey = toSqlKey uid

checkUserPass :: String -> String -> IO (Maybe User)
checkUserPass email password = do
  user' <- liftIO $ (runAction connString (getBy $ UniqueEmail email))
  case user' of
    Just (Entity _ user) -> 
      if userPassword user == password 
      then return (Just user)
      else return Nothing
    Nothing -> return Nothing


createMovie :: Movie -> IO Int64
createMovie movie = fromSqlKey <$> runAction connString (insert movie)

readMovie :: Int64 -> IO (Maybe Movie)
readMovie mid = runAction connString (get movieKey)
  where
    movieKey :: Key Movie
    movieKey = toSqlKey mid

getAllMovie :: IO [Entity Movie]
getAllMovie = runAction connString (selectList [] [])

updateMovie :: Int64 -> Movie -> IO ()
updateMovie mid updatedMovie = runAction connString $ do
    maybeMovie <- get movieKey
    case maybeMovie of
        Nothing -> liftIO $ putStrLn "Movie not found."
        Just _ -> replace (toSqlKey mid) updatedMovie
    where
      movieKey :: Key Movie
      movieKey = toSqlKey mid

deleteMovie :: Int64 -> IO ()
deleteMovie mid = runAction connString (delete movieKey)
  where
    movieKey :: Key Movie
    movieKey = toSqlKey mid
