{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}

module Api (api, API)
where

import Data.Int (Int64)

import Models
import Servant

type API = "movie" :> BasicAuth "user" User :> ReqBody '[JSON] Movie :> Post '[JSON] Movie
       :<|> "movie" :> Capture "id" Int64 :> Get '[JSON] Movie
       :<|> "movie" :> BasicAuth "user" User :> Capture "id" Int64 :> ReqBody '[JSON] Movie :> Put '[JSON] NoContent
       :<|> "movie" :> BasicAuth "user" User :> Capture "id" Int64 :> DeleteNoContent 
       :<|> "movies" :> Get '[JSON] [Movie]
       :<|> "fav" :> BasicAuth "user" User :> Capture "id" Int64 :> Post '[JSON] NoContent
       :<|> "fav" :> BasicAuth "user" User :> Capture "id" Int64 :> DeleteNoContent

       :<|> "user" :> ReqBody '[JSON] User :> Post '[JSON] User
       :<|> "user" :> BasicAuth "user" User :> Capture "id" Int64 :> Get '[JSON] User
       :<|> "user" :> BasicAuth "user" User :> Capture "id" Int64 :> ReqBody '[JSON] User :> Put '[JSON] NoContent
       :<|> "user" :> BasicAuth "user" User :> Capture "id" Int64 :> DeleteNoContent


api :: Proxy API
api = Proxy
