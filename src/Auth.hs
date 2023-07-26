{-# LANGUAGE DataKinds             #-}

module Auth (basicAuthServerContext)
where


import Servant
import Servant.Server.Experimental.Auth()
import Database (checkUserPass)
import Data.ByteString.Char8 (unpack)
import Models (User)


basicAuthServerContext :: Context '[BasicAuthCheck User]
basicAuthServerContext = BasicAuthCheck check :. EmptyContext
  where 
  check :: BasicAuthData -> IO (BasicAuthResult User)
  check (BasicAuthData username password) = do
    user' <- checkUserPass (unpack username) (unpack password)
    case user' of
      Just user -> return (Authorized user)
      Nothing -> return Unauthorized
