{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Lib
  ( startApp
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp
import Servant hiding (Unauthorized, Authorized)
import System.Directory
import Servant.Server.Experimental.Auth()
import Codec.Binary.UTF8.Generic (fromString)
import Servant.Server.Internal.BasicAuth
import Data.ByteString (ByteString)
import Database (createMovie, readMovie, updateMovie, deleteMovie, getAllMovie)
import Data.Text (Text)
import Data.Int (Int64)
import Database
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.Sql


type API = "movie" :> BasicAuth "admin" User :> ReqBody '[JSON] Movie :> Post '[JSON] Movie
       :<|> "movie" :> BasicAuth "admin" User :> Capture "id" Int64 :> DeleteNoContent 
       :<|> "movie" :> BasicAuth "admin" User :> Capture "id" Int64 :> Get '[JSON] Movie
       :<|> "movie" :> BasicAuth "admin" User :> Capture "id" Int64 :> ReqBody '[JSON] Movie :> Put '[JSON] NoContent
       :<|> "movies" :> Get '[JSON] [Movie]

hPostMovie :: User -> Movie -> Handler Movie
hPostMovie user movie = do
  liftIO $ print "adding movie into db"
  key <- liftIO $ createMovie movie
  liftIO $ print "Successfully added movie"
  movie' <- liftIO $ readMovie key
  case movie' of 
    Just m -> return m
    Nothing -> throwError (err404 {errBody = fromString ("Error adding movie")})

hDeleteMovie :: User -> Int64 -> Handler NoContent
hDeleteMovie user mid = do
  liftIO $ deleteMovie mid
  return NoContent

hGetMovie :: User -> Int64 -> Handler Movie
hGetMovie user mid = do
  movie <- liftIO $ readMovie mid
  case movie of 
    Just m -> return m
    Nothing -> throwError (err404 {errBody = fromString (" doesnt exist")})

hUpdateMovie :: User -> Int64 -> Movie -> Handler NoContent
hUpdateMovie user mid movie = do
  liftIO $ updateMovie mid movie
  return NoContent
  
hGetMovies :: Handler [Movie]
hGetMovies = do
  movies <- liftIO $ getAllMovie
  return $ map entityVal movies


authUser :: ByteString -> ByteString -> Maybe User
authUser username password = 
  if username == "servant" && password == "server" 
    then Just (User "servant" "servant@gmail.com")
    else Nothing

basicAuthServerContext :: Context '[BasicAuthCheck User]
basicAuthServerContext = BasicAuthCheck check :. EmptyContext
  where 
  check :: BasicAuthData -> IO (BasicAuthResult User)
  check (BasicAuthData username password) = do
    case authUser username password of
      Just user -> return (Authorized user)
      Nothing -> return Unauthorized

api :: Proxy API
api = Proxy

server :: Server API
server = hPostMovie
    :<|> hDeleteMovie
    :<|> hGetMovie
    :<|> hUpdateMovie
    :<|> hGetMovies

startApp :: IO ()
startApp =
  Network.Wai.Handler.Warp.run
    8080
    ( serveWithContext
        api
        basicAuthServerContext
        server
    )
