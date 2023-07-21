module Main (main) where

import Data.Text (Text, pack, unpack)
import Data.Int (Int64)
import Lib
import Database


user = User {
    userName = pack "Dhruv",
    userEmail = pack "dhruv1.jain@gmail.com"
}

user1 = User {
    userName = pack "My name",
    userEmail = pack "abc1@gmail.com"
}

main :: IO ()
main = do
  -- migrateDB
  print "Starting server on port 8080"
  startApp
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
