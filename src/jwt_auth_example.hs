{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Auth 
where

import Data.Aeson
import GHC.Generics
import Data.Proxy
import System.IO
import Network.Wai.Handler.Warp
import Servant as S
import Servant.Auth as SA
import Servant.Auth.Server as SAS
import Control.Monad.IO.Class (liftIO)
import Data.Map as M
import Data.ByteString (ByteString)
import Config
import Data.ByteString.Char8 (unpack)
import Database
import Models


instance ToJWT User
instance FromJWT User

authCheck :: BasicAuthData
          -> IO (AuthResult User)
authCheck (BasicAuthData login password) = do
  user' <- checkUserPass (unpack login) (unpack password)
  case user' of
    Nothing -> return SAS.Indefinite
    Just user -> return (Authenticated user)

type instance BasicAuthCfg = BasicAuthData -> IO (AuthResult User)

instance FromBasicAuthData User where
  fromBasicAuthData authData authCheckFunction = authCheckFunction authData

type API = "auth" :> Get '[JSON] ()
      :<|> "bar" :> Get '[JSON] ()

type APIServer =
  Auth '[SA.JWT, SA.BasicAuth] User :> API


handleAuth :: Handler ()
handleAuth = liftIO $ hPutStrLn stderr $ "auth"

handleBar :: Handler ()
handleBar = liftIO $ putStrLn "Handling bar request"

server :: Server APIServer
server (Authenticated user)= handleAuth :<|> handleBar
server _ = throwAll err401

app :: IO Application
app = do
  myKey <- generateKey
  let authCfg = authCheck
      jwtCfg = defaultJWTSettings myKey
      cfg = jwtCfg :. defaultCookieSettings :. authCfg :. EmptyContext
      api = Proxy :: Proxy APIServer
  pure $ serveWithContext api cfg server
