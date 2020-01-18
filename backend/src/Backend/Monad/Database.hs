{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Backend.Monad.Database where

import Control.Monad (Monad)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Logger (MonadLogger)
import Crypto.PasswordStore (makePassword, verifyPassword)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Database.Esqueleto
import Database.Persist (Entity(..), get)
import Database.Persist.Postgresql (SqlPersistT, fromSqlKey)

import Common.Schema
import Backend.Database

class (Monad m) => MonadDatabase m where
  createUser :: (User, Text) -> m Int64
  loginUser :: LoginInfo -> Text -> m (Either LoginError Int64)
  getCurrentEvents :: m [KeyVal Event]
  getPurchasedEvents :: Int64 -> m [(Event, [EventTicket])]
  createEvent :: Event -> m Int64
  fetchEvent :: Int64 -> m (Maybe Event)
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
  fetchEvent = fetchEventSql
  createTicketsForEvent = createTicketsForEventSql
  getCreatedEvents = getCreatedEventsSql
  getEventSummary = getEventSummarySql
  deleteEvent = deleteEventSql
  purchaseTicket = purchaseTicketSql

createUserSql :: (MonadIO m, MonadLogger m) => (User, Text) -> SqlPersistT m Int64
createUserSql (user, password) = do
  userKey <- insert user
  passwordHash <- liftIO $ makePassword (encodeUtf8 password) 17
  insert (AuthData userKey passwordHash "user")
  return (fromSqlKey userKey)

loginUserSql :: (MonadIO m, MonadLogger m) => LoginInfo -> Text -> SqlPersistT m (Either LoginError Int64)
loginUserSql (LoginInfo email password) role = do
  userAuthData <- select . from $ \(users `InnerJoin` authData) -> do
    on (users ^. UserId ==. authData ^. AuthDataUserId)
    where_ (users ^. UserEmail ==. val email)
    where_ (authData ^. AuthDataUserRole ==. val role)
    return authData
  case userAuthData of
    [Entity _ (AuthData uid hash _)] -> do
      if verifyPassword (encodeUtf8 password) hash
        then return (Right $ fromSqlKey uid)
        else return (Left InvalidPassword)
    _ -> return (Left InvalidUsername)

getCurrentEventsSql :: (MonadIO m, MonadLogger m) => SqlPersistT m [KeyVal Event]
getCurrentEventsSql = undefined

getPurchasedEventsSql :: (MonadIO m, MonadLogger m) => Int64 -> SqlPersistT m [(Event, [EventTicket])]
getPurchasedEventsSql = undefined

createEventSql :: (MonadIO m, MonadLogger m) => Event -> SqlPersistT m Int64
createEventSql = undefined

fetchEventSql :: (MonadIO m, MonadLogger m) => Int64 -> SqlPersistT m (Maybe Event)
fetchEventSql eid = get (toSqlKey eid)

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
