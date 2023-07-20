module Main (main) where

import Data.Text (Text, pack, unpack)
import Lib
import Database


user = User {
    userName = pack "Dhruv",
    userEmail = pack "dhruv.jain@gmail.com"
}

main :: IO ()
main = do
  let user = (getUser 1)
  user' <- user
  case user' of 
    Just u -> putStrLn (show u)
    Nothing -> putStrLn "hello dhruv"
  deleteUser 1
