module Main (main) where

import Lib
import Database

main :: IO ()
-- main = startApp
main = migrateDB
