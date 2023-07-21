module Main (main) where

import Auth
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Map as M
import Data.Proxy
import Data.Text (Text, pack, unpack)
import Database
import GHC.Generics
import Lib
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.Wai.Handler.Warp
import Servant as S
import Servant.Auth as SA
import Servant.Auth.Server as SAS
import Servant.Client
import System.IO


user = User {
    userName = pack "Dhruv",
    userEmail = pack "dhruv1.jain@gmail.com"
}

user1 = User {
    userName = pack "My name",
    userEmail = pack "abc1@gmail.com"
}

-- main :: IO ()
-- main = do
  -- migrateDB
  -- print "Starting server on port 8080"
  -- startApp
  -- key <- createUser user
  -- let user = (getUser key)
  -- user' <- user
  -- case user' of 
  --   Just u -> putStrLn (show u)
  --   Nothing -> putStrLn "user not found"
  -- updateUser key user1
  -- let user = (getUser key)
  -- user' <- user
  -- case user' of 
  --   Just u -> putStrLn (show u)
  --   Nothing -> putStrLn "user not found"
  -- deleteUser key
  -- entities <- getAllUser
  -- mapM_ print entities
  --
main :: IO ()
main = do
  connPool <- initConnPool
  let settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr
                           ("listening on port " ++ show port)) $
        defaultSettings
  runSettings settings =<< mkApp connPool
