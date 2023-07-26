module Main (main)
where

import Database (migrateDB)
import App (startApp)

-- user = User {
--     userName = pack "dhruv",
--     userEmail = pack "dhruv1.jain@gmail.com",
--     userPassword = Just (pack "pass"),
--     userToken = Nothing
--     userId = Nothing
-- }

main :: IO ()
main = do
  migrateDB
  startApp
