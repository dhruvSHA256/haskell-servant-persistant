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
  ( startApp,
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

data FileContent = FileContent
  { content :: String
  }
  deriving (Eq, Show, Generic)
instance ToJSON FileContent

newtype User = User {userName :: String}
  deriving (Eq, Show, Generic)
instance ToJSON User

type API = "content" :> BasicAuth "admin" User :> Capture "fileName" FilePath :> Get '[JSON] FileContent

hIO :: User -> FilePath -> Handler FileContent
hIO user fileName = do
  -- liftIO $ print user
  exist <- liftIO (doesFileExist fileName)
  liftIO $ putStrLn fileName
  if exist
    then do
      liftIO (readFile fileName) >>= return . FileContent
    else throwError customError
  where
    customError = err404 {errBody = fromString (fileName ++ " doesnt exist")}


authUser :: ByteString -> ByteString -> Maybe User
authUser username password = 
  if username == "servant" && password == "server1" 
    then Just (User "servant")
    else Nothing

basicAuthServerContext :: Context '[BasicAuthCheck User]
basicAuthServerContext = BasicAuthCheck check :. EmptyContext
  where 
  check :: BasicAuthData -> IO (BasicAuthResult User)
  check (BasicAuthData username password) = do
    -- print username
    -- print password
    case authUser username password of
      Just user -> return (Authorized user)
      Nothing -> return Unauthorized

api :: Proxy API
api = Proxy

server :: Server API
server = hIO

startApp :: IO ()
startApp =
  Network.Wai.Handler.Warp.run
    8080
    ( serveWithContext
        api
        basicAuthServerContext
        server
    )
