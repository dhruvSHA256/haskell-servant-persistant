module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Map as M
import Data.Proxy
import Data.Text (Text, pack, unpack)
import GHC.Generics
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.Wai.Handler.Warp
import Servant as S
import Servant.Auth as SA
import Servant.Auth.Server as SAS
import Servant.Client
import System.IO
import Auth
import Config
import Database
import Lib

-- user = User {
--     userName = pack "dhruv",
--     userEmail = pack "dhruv1.jain@gmail.com",
--     userPassword = Just (pack "pass"),
--     userToken = Nothing
-- }

-- user1 = User {
--     userName = "My name",
--     userEmail = "abc",
--     userPassword = "abc",
--     userToken = Nothing
-- }

main :: IO ()
main = do
  -- migrateDB
  -- liftIO $ (createUser user)
  -- liftIO $ (createUser user1)
  let settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr
                           ("listening on port " ++ show port)) $
        defaultSettings
  runSettings settings =<< app
