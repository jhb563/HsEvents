{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Backend.Database where

import Control.Exception (Exception)
import Control.Monad.Logger (runStdoutLoggingT, LoggingT, LogLevel(..), filterLogger)
import Control.Monad.Reader (runReaderT)
import Data.Aeson
import Data.Int (Int64)
import Database.Persist.Sql (Entity, entityKey, entityVal, fromSqlKey, ToBackendKey, SqlBackend)
import Database.Persist.Postgresql (ConnectionString, withPostgresqlConn, SqlPersistT)

data KeyVal a = KeyVal
  { kvKey :: Int64
  , kvVal :: a
  } deriving (Show, Eq)

entityToKeyVal :: (ToBackendKey SqlBackend a) => Entity a -> KeyVal a
entityToKeyVal e = KeyVal (fromSqlKey . entityKey $ e) (entityVal e)

instance (FromJSON a) => FromJSON (KeyVal a) where
  parseJSON = withObject "Keyval" $ \o -> do
    k <- o .: "key"
    v <- o .: "value"
    return $ KeyVal k v

instance (ToJSON a) => ToJSON (KeyVal a) where
  toJSON kv = object
    [ "key" .= kvKey kv
    , "value" .= kvVal kv
    ]

data LoginError = InvalidUsername | MissingAuthData | InvalidPassword
  deriving (Show, Eq)

instance Exception LoginError

localConnString :: ConnectionString
localConnString = "host='localhost' port=5432 dbname='mmh' user='postgres' password='postgres'"

runSqlAction :: ConnectionString -> SqlPersistT (LoggingT IO) a -> IO a
runSqlAction connString action =
  runStdoutLoggingT $ filterLogger logFilter $ withPostgresqlConn connString $ \backend ->
    runReaderT action backend

logFilter :: a -> LogLevel -> Bool
logFilter _ LevelDebug = False
logFilter _ (LevelOther _) = False
logFilter _ _ = True
