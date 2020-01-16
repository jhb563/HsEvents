{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Backend.Monad.Database where

import Control.Monad (Monad)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Logger (MonadLogger)
import Data.Int (Int64)
import Data.Text (Text)
import Database.Persist.Postgresql (SqlPersistT)

import Common.Schema
import Backend.Database

class (Monad m) => MonadDatabase m where
  createUser :: (User, Text) -> m Int64
  loginUser :: LoginInfo -> m (Either LoginError Int64)
  getCurrentEvents :: m [KeyVal Event]
  getPurchasedEvents :: Int64 -> m [(Event, [EventTicket])]
  createEvent :: Event -> m Int64
  createTicketsForEvent :: Int64 -> [(Text, Double, Int64)] -> m [Int64]
  getCreatedEvents :: Int64 -> m [KeyVal Event]
  getEventSummary :: Int64 -> m EventSummary
  deleteEvent :: Int64 -> m ()
  purchaseTicket :: Int64 -> Int64 -> m ()

instance (MonadLogger m, MonadIO m) => MonadDatabase (SqlPersistT m) where
  createUser = createUserSql
  loginUser = loginUserSql
  getCurrentEvents = getCurrentEventsSql
  getPurchasedEvents = getPurchasedEventsSql
  createEvent = createEventSql
  createTicketsForEvent = createTicketsForEventSql
  getCreatedEvents = getCreatedEventsSql
  getEventSummary = getEventSummarySql
  deleteEvent = deleteEventSql
  purchaseTicket = purchaseTicketSql

createUserSql :: (MonadIO m, MonadLogger m) => (User, Text) -> SqlPersistT m Int64
createUserSql = undefined

loginUserSql :: (MonadIO m, MonadLogger m) => LoginInfo -> SqlPersistT m (Either LoginError Int64)
loginUserSql = undefined

getCurrentEventsSql :: (MonadIO m, MonadLogger m) => SqlPersistT m [KeyVal Event]
getCurrentEventsSql = undefined

getPurchasedEventsSql :: (MonadIO m, MonadLogger m) => Int64 -> SqlPersistT m [(Event, [EventTicket])]
getPurchasedEventsSql = undefined

createEventSql :: (MonadIO m, MonadLogger m) => Event -> SqlPersistT m Int64
createEventSql = undefined

createTicketsForEventSql :: (MonadIO m, MonadLogger m) => Int64 -> [(Text, Double, Int64)] -> SqlPersistT m [Int64]
createTicketsForEventSql = undefined

getCreatedEventsSql :: (MonadIO m, MonadLogger m) => Int64 -> SqlPersistT m [KeyVal Event]
getCreatedEventsSql = undefined

getEventSummarySql :: (MonadIO m, MonadLogger m) => Int64 -> SqlPersistT m EventSummary
getEventSummarySql = undefined

deleteEventSql :: (MonadIO m, MonadLogger m) => Int64 -> SqlPersistT m ()
deleteEventSql = undefined

purchaseTicketSql :: (MonadIO m, MonadLogger m) => Int64 -> Int64 -> SqlPersistT m ()
purchaseTicketSql = undefined
