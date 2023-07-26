{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances #-}

module Models
where


import qualified Database.Persist.TH as PTH
import Data.Aeson (FromJSON, ToJSON(toJSON), parseJSON, (.:), (.:?), (.=), object, withObject)
import GHC.Generics (Generic)

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
  FavouritMovies sql=fav_movies
    movie MovieId
    user UserId
    UniqueFavMovie user movie
    deriving Show Read Generic
|]


instance ToJSON Movie where
  toJSON (Movie name rating genre user) = object $
    ["name" .= name] ++
    maybe [] (\r -> ["rating" .= r]) rating ++
    maybe [] (\g -> ["genre" .= g]) genre ++
    maybe [] (\u -> ["user" .= u]) user

instance FromJSON Movie where
  parseJSON = withObject "Movie" $ \obj ->
    Movie <$> obj .: "name"
          <*> obj .:? "rating"
          <*> obj .:? "genre"
          <*> obj .:? "user"


instance ToJSON User where
  toJSON (User name email password token) = object $
    ["name" .= name] ++
    ["email" .= email] ++
    ["password" .= password] ++
    maybe [] (\t -> ["token" .= t]) token

instance FromJSON User where
  parseJSON = withObject "User" $ \obj ->
    User
      <$> obj .: "name"
      <*> obj .: "email"
      <*> obj .: "password"
      <*> obj .:? "token"
