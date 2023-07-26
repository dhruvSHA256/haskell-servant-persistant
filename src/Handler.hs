module Handler 
  ( hPostUser,
    hGetUser,
    hPutUser,
    hDeleteUser,
    hPostMovie,
    hGetMovie,
    hPutMovie,
    hDeleteMovie,
    hListMovie,
    hPostFavMovie,
    hDeleteFavMovie,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Int (Int64)
import Database.Persist.Sql
import Codec.Binary.UTF8.Generic (fromString)

import Models
import Servant
import Database


hPostUser :: User -> Handler User 
hPostUser user_ = do
  liftIO $ print "Creating new user"
  liftIO $ print user_
  let user = user_ { userToken = Nothing }
  liftIO $ print user
  uid <- liftIO $ createUser user
  liftIO $ print "Successfully created user"
  user' <- liftIO $ readUser uid
  case user' of 
    Just u -> return u
    Nothing -> throwError (err404 {errBody = fromString ("Error creating user")})


hGetUser :: User -> Int64 -> Handler User
hGetUser _user uid = do
  user' <- liftIO $ readUser uid
  case user' of 
    Just user -> 
      if (userEmail user) == (userEmail _user) then
        return user 
      else throwError (err403 {errBody = fromString ("Forbidden")})
    Nothing -> throwError (err404 {errBody = fromString ("User not found")})


hPutUser :: User -> Int64 -> User -> Handler NoContent
hPutUser _user uid user = do
  user' <- liftIO $ readUser uid
  case user' of 
    Just user_ -> 
      if (userEmail user_) == (userEmail _user) then
        liftIO $ updateUser uid user
      else throwError (err403 {errBody = fromString ("Forbidden")})
    Nothing -> throwError (err404 {errBody = fromString ("User not found")})
  return NoContent


hDeleteUser :: User -> Int64 -> Handler NoContent
hDeleteUser _user uid = do
  user' <- liftIO $ readUser uid
  case user' of 
    Just user -> 
      if (userEmail user) == (userEmail _user) then
        liftIO $ deleteUser uid
      else throwError (err403 {errBody = fromString ("Forbidden")})
    Nothing -> throwError (err404 {errBody = fromString ("User not found")})
  return NoContent


hPostMovie :: User -> Movie -> Handler Movie
hPostMovie user movie' = do
  (Just (Entity x _)) <- liftIO $ readEntityByEmail (userEmail user)
  let _movie = movie' { movieUser = (Just x)}
  mid <- liftIO $ createMovie _movie
  m <- liftIO $ readMovie mid
  case m of 
    Just movie -> do
      return movie
    Nothing -> throwError (err404 {errBody = fromString ("Error adding movie")})


hGetMovie :: Int64 -> Handler Movie
hGetMovie _ mid = do
  movie <- liftIO $ readMovie mid
  case movie of 
    Just m -> return m
    Nothing -> throwError (err404 {errBody = fromString ("Movie not found")})


hPutMovie :: User -> Int64 -> Movie -> Handler NoContent
hPutMovie user mid movie = do
  maybeuser <- liftIO $ getMovieCreater mid
  case maybeuser of
    Just user' -> 
      if (userEmail user') == (userEmail user)
      then liftIO $ updateMovie mid movie
      else throwError (err403 {errBody = fromString ("Forbidden")})
    Nothing -> throwError (err404 {errBody = fromString ("Movie not found")})
  return NoContent


hDeleteMovie :: User -> Int64 -> Handler NoContent
hDeleteMovie user mid = do
  maybeuser <- liftIO $ getMovieCreater mid
  case maybeuser of
    Just user' -> 
      if (userEmail user') == (userEmail user)
      then liftIO $ deleteMovie mid
      else throwError (err403 {errBody = fromString ("Forbidden")})
    Nothing -> throwError (err404 {errBody = fromString ("Movie not found")})
  return NoContent


hListMovie :: Handler [Movie]
hListMovie = do
  movies <- liftIO $ getAllMovie
  return $ map entityVal movies


hPostFavMovie :: User -> Int64 -> Handler NoContent
hPostFavMovie user mid = do
  let email = userEmail user
  _ <- liftIO $ createFavMovie email mid
  return NoContent


hDeleteFavMovie :: User -> Int64 -> Handler NoContent
hDeleteFavMovie user mid = do
  let email = userEmail user
  liftIO $ deleteFavMovie email mid
  return NoContent
