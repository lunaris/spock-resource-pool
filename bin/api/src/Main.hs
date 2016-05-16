{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Main (main) where

import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.Control as Ctrl
import qualified Control.Monad.Trans.Reader as Rdr
import qualified Data.Pool as Pool
import qualified Database.PostgreSQL.Simple as PG
import qualified System.Environment as Env
import qualified Web.Spock as Spock

newtype PostgreSQLT m a
  = PostgreSQLT (Rdr.ReaderT PG.Connection m a)
  deriving (Applicative, Functor, Monad)

newtype PostgreSQLConnectionPool
  = PostgreSQLConnectionPool (Pool.Pool PG.Connection)

runPostgreSQLT  :: (Ctrl.MonadBaseControl IO m, Monad m)
                => PostgreSQLConnectionPool
                -> PostgreSQLT m a
                -> m a

runPostgreSQLT (PostgreSQLConnectionPool pool) (PostgreSQLT m)
  = Pool.withResource pool (Rdr.runReaderT m)

class Monad m => MonadPostgreSQL m where
  query_ :: PG.FromRow a => PG.Query -> m [a]

instance IO.MonadIO m => IO.MonadIO (PostgreSQLT m) where
  liftIO
    = PostgreSQLT . Trans.lift . IO.liftIO

instance IO.MonadIO m => MonadPostgreSQL (PostgreSQLT m) where
  query_ sql = PostgreSQLT $ do
    conn <- Rdr.ask
    IO.liftIO (PG.query_ conn sql)

createPostgreSQLConnectionPool :: IO PostgreSQLConnectionPool
createPostgreSQLConnectionPool = do
  username <- Env.getEnv "PG_USERNAME"
  password <- Env.getEnv "PG_PASSWORD"
  database <- Env.getEnv "PG_DATABASE"

  let connectToPostgreSQL = do
        putStrLn "Creating PostgreSQL connection"
        PG.connect PG.defaultConnectInfo
          { PG.connectUser      = username
          , PG.connectPassword  = password
          , PG.connectDatabase  = database
          }

      closePostgreSQL = \conn -> do
        putStrLn "Closing PostgreSQL connection"
        PG.close conn

  PostgreSQLConnectionPool <$> Pool.createPool
    connectToPostgreSQL
    closePostgreSQL
    1   --  Number of stripes in the pool.
    10  --  Number of seconds a connection must be idle before being reaped.
    10  --  Maximum number of connections per stripe.

main :: IO ()
main = do
  pool <- createPostgreSQLConnectionPool

  Spock.runSpock 8080 $ Spock.spockT (runPostgreSQLT pool) $ do
    Spock.get "/test" $ do
      [PG.Only one] <- Trans.lift $ query_ "SELECT 1"
      Spock.json (one :: Int)
