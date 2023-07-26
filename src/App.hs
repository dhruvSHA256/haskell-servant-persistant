{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}


module App
  ( startApp
  )
where

import Network.Wai.Handler.Warp
import Servant

import Config (port)
import Api (api, API)
import Auth
import Handler
 

server :: Server API
server = hPostMovie
    :<|> hGetMovie
    :<|> hPutMovie
    :<|> hDeleteMovie
    :<|> hListMovie
    :<|> hPostFavMovie
    :<|> hDeleteFavMovie
    :<|> hPostUser
    :<|> hGetUser
    :<|> hPutUser
    :<|> hDeleteUser

startApp :: IO ()
startApp =
  Network.Wai.Handler.Warp.run
    port
    ( serveWithContext
        api
        basicAuthServerContext
        server
    )
