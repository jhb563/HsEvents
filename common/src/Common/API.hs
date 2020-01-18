{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Common.API where

import Data.Int (Int64)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Servant.API

import Common.Schema

type UsersApi = "api" :> "users" :>
  ( "create" :> ReqBody '[JSON] (User, Text) :> Post '[JSON] Int64 :<|>
    "login" :> ReqBody '[JSON] LoginInfo :> Post '[JSON] (Maybe Int64)
  )

type EventsApi = "api" :> "events" :>
  ( BasicAuth "user" NormalUserId :> "browse" :> Get '[JSON] [(Int64, Event)] :<|>
    BasicAuth "user" NormalUserId :> "purchased" :> Get '[JSON] [(Event, [EventTicket])] :<|>
    BasicAuth "creator" CreatorUserId :> "create" :> ReqBody '[JSON] EventSummary :> Post '[JSON] Int64 :<|>
    BasicAuth "creator" CreatorUserId :> "summary" :> Get '[JSON] [(Int64, Event)] :<|>
    BasicAuth "creator" CreatorUserId :> "summary" :> Capture "event-id" Int64 :> Get '[JSON] (Maybe EventSummary) :<|>
    BasicAuth "creator" CreatorUserId :> "delete" :> Capture "event-id" Int64 :> Delete '[JSON] ()
  )

type PurchaseApi = "api" :> "purchases" :>
  ( BasicAuth "user" NormalUserId :> "enter-queue" :> Capture "event-id" Int64 :> Get '[JSON] () :<|>
    BasicAuth "user" NormalUserId :> "purchase" :> Capture "event-id" Int64 :> ReqBody '[JSON] [(Text, Int64)] :> Post '[JSON] ()
  )

type FullApi = UsersApi :<|> EventsApi :<|> PurchaseApi

fullApi :: Proxy FullApi
fullApi = Proxy
