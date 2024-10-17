{-# LANGUAGE UndecidableInstances #-}

module Conduit.DB.Init where

import Conduit.DB.Core (DBPool(..))
import Conduit.Features.Account.DB (migrateAccountTables)
import Conduit.Features.Articles.DB (createArticleFunctions, migrateArticleTables)
import Database.Esqueleto.Experimental (SqlPersistT, createPoolConfig, rawExecute, runMigration, runSqlPool)
import Database.Persist.Postgresql (PostgresConf(..))
import UnliftIO (MonadUnliftIO)

data PGConnOps = PGConnOps
  { connStr     :: !Text
  , connSize    :: !Int
  , connTimeout :: !Int
  , connStripes :: !Int
  , truncTables :: !Bool
  } deriving (Read, Show)

mkPoolConfig :: PGConnOps -> PostgresConf
mkPoolConfig ops = PostgresConf
  { pgConnStr         = fromString $ toString ops.connStr
  , pgPoolSize        = ops.connSize
  , pgPoolIdleTimeout = fromIntegral ops.connTimeout
  , pgPoolStripes     = ops.connStripes
  }

mkDBPool :: (MonadIO m) => PGConnOps -> m DBPool
mkDBPool = liftIO . fmap DBPool . createPoolConfig . mkPoolConfig

initDB :: (MonadUnliftIO m) => DBPool -> PGConnOps -> m ()
initDB (DBPool pool) ops = flip runSqlPool pool do
  print ops
  when ops.truncTables resetTables
  runMigrations
  runDBFunctions

tables :: [Text]
tables = ["user", "follow", "article", "favorite", "comment"]

dropTables :: (MonadIO m) => SqlPersistT m ()
dropTables = flip rawExecute [] $
  foldMap (\t -> "drop table if exists  \"" <> t <> "\" cascade;") tables

resetTables :: (MonadIO m) => SqlPersistT m ()
resetTables = flip rawExecute [] $
  foldMap (\t -> "truncate \"" <> t <> "\" cascade;") tables

runMigrations :: (MonadIO m) => SqlPersistT m ()
runMigrations = do
  putStrLn "runnning migrations..."
  runMigration migrateAccountTables
  runMigration migrateArticleTables

runDBFunctions :: (MonadIO m) => SqlPersistT m ()
runDBFunctions = do
  createArticleFunctions
