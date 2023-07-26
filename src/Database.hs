module Database
  (
    createUser,
    readUser,
    updateUser,
    deleteUser,
    readEntityByEmail,
    getAllUser,
    createMovie,
    readMovie,
    updateMovie,
    deleteMovie,
    createFavMovie,
    deleteFavMovie,
    getAllMovie,
    checkUserPass,
    getMovieCreater,
    migrateDB,
  )
where

import Control.Monad.Reader (runReaderT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger
import Database.Persist
import Database.Persist.Postgresql
import Data.Int (Int64)

import Config
import Models


runAction :: ConnectionString -> SqlPersistT (LoggingT IO) a ->  IO a
runAction connectionString action = runStdoutLoggingT $ withPostgresqlConn connectionString $ \backend ->
  runReaderT action backend

migrateDB :: IO ()
migrateDB = runAction connString (runMigration migrateAll)

createUser :: User -> IO Int64
createUser user = fromSqlKey <$> runAction connString (insert user)

readUser :: Int64 -> IO (Maybe User)
readUser uid = runAction connString (get userKey)
  where
    userKey :: Key User
    userKey = toSqlKey uid

readEntityByEmail :: String -> IO (Maybe (Entity User))
readEntityByEmail email = do 
  user <- liftIO $ (runAction connString (getBy $ UniqueEmail email))
  return user

updateUser :: Int64 -> User -> IO ()
updateUser uid updatedUser = runAction connString $ do
    maybeUser <- get userKey
    case maybeUser of
        Nothing -> liftIO $ putStrLn "User not found."
        Just _ -> replace (toSqlKey uid) updatedUser
    where
      userKey :: Key User
      userKey = toSqlKey uid

deleteUser :: Int64 -> IO ()
deleteUser uid = runAction connString (delete userKey)
  where
    userKey :: Key User
    userKey = toSqlKey uid

getAllUser :: IO [Entity User]
getAllUser = runAction connString (selectList [] [])

checkUserPass :: String -> String -> IO (Maybe User)
checkUserPass email password = do
  user' <- liftIO $ (runAction connString (getBy $ UniqueEmail email))
  case user' of
    Just (Entity _ user) -> 
      if userPassword user == password 
      then return (Just user)
      else return Nothing
    Nothing -> return Nothing

createMovie :: Movie -> IO Int64
createMovie movie = fromSqlKey <$> runAction connString (insert movie)

readMovie :: Int64 -> IO (Maybe Movie)
readMovie mid = runAction connString (get movieKey)
  where
    movieKey :: Key Movie
    movieKey = toSqlKey mid

updateMovie :: Int64 -> Movie -> IO ()
updateMovie mid updatedMovie = runAction connString $ do
    maybeMovie <- get movieKey
    case maybeMovie of
        Nothing -> liftIO $ putStrLn "Movie not found."
        Just _ -> replace (toSqlKey mid) updatedMovie
    where
      movieKey :: Key Movie
      movieKey = toSqlKey mid


deleteMovie :: Int64 -> IO ()
deleteMovie mid = runAction connString (delete movieKey)
  where
    movieKey :: Key Movie
    movieKey = toSqlKey mid

createFavMovie :: String -> Int64 -> IO Int64
createFavMovie email mid = runAction connString $ do
    maybeMovie <- get movieKey
    (Just (Entity uid _)) <- getBy $ UniqueEmail email
    case maybeMovie of
        Nothing -> do
          liftIO $ putStrLn "Movie not found."
          return (-1)
        Just _ -> do 
          let favmovie = FavouritMovies { favouritMoviesUser = uid , favouritMoviesMovie = toSqlKey mid}
          favmoviekey <- insert favmovie
          return (fromSqlKey favmoviekey)
    where
      movieKey :: Key Movie
      movieKey = toSqlKey mid
          
deleteFavMovie :: String -> Int64 -> IO ()
deleteFavMovie email mid = runAction connString $ do
    maybeMovie <- get movieKey
    (Just (Entity uid _)) <- getBy $ UniqueEmail email
    case maybeMovie of
        Nothing -> do
          liftIO $ putStrLn "Movie not found."
        Just _ -> do 
          deleteWhere [FavouritMoviesUser ==. uid , FavouritMoviesMovie ==. (toSqlKey mid)]
    where
      movieKey :: Key Movie
      movieKey = toSqlKey mid

getAllMovie :: IO [Entity Movie]
getAllMovie = runAction connString (selectList [] [])


getMovieCreater :: Int64 -> IO (Maybe User)
getMovieCreater mid = runAction connString $ do
    maybeMovie <- get movieKey
    case maybeMovie of 
      Nothing -> do
        liftIO $ putStrLn "Movie not found."
        return Nothing
      Just movie -> do
        case (movieUser movie) of
          (Just userkey) -> do
            (Just user) <- get userkey
            return (Just user)
          Nothing -> 
            return Nothing
    where
      movieKey :: Key Movie
      movieKey = toSqlKey mid
