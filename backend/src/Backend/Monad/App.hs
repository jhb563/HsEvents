{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Backend.Monad.App where

import Control.Exception (throwIO)
import Control.Monad.Catch (MonadThrow(..))
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Logger (LoggingT)
import Control.Monad.Trans (lift)
import Database.Persist.Postgresql (SqlPersistT)

import Backend.Monad.Database (MonadDatabase(..))

newtype ServerMonad a = ServerMonad
  (SqlPersistT (LoggingT IO) a)
  deriving (Functor, Applicative, Monad)

instance MonadIO ServerMonad where
  liftIO action = ServerMonad (lift . lift $ action)

instance MonadThrow ServerMonad where
  throwM e = liftIO (throwIO e)

liftSqlToServer :: SqlPersistT (LoggingT IO) a -> ServerMonad a
liftSqlToServer = ServerMonad

instance MonadDatabase ServerMonad where
  createUser = liftSqlToServer . createUser
  loginUser = liftSqlToServer . loginUser
  getCurrentEvents = liftSqlToServer getCurrentEvents
  getPurchasedEvents = liftSqlToServer . getPurchasedEvents
  createEvent = liftSqlToServer . createEvent
  createTicketsForEvent eid tiers = liftSqlToServer (createTicketsForEvent eid tiers)
  getCreatedEvents = liftSqlToServer . getCreatedEvents
  getEventSummary = liftSqlToServer . getEventSummary
  deleteEvent = liftSqlToServer . deleteEvent
  purchaseTicket uid etid = liftSqlToServer (purchaseTicket uid etid)
