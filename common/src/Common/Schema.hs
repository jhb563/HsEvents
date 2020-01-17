{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Common.Schema where

import Data.Aeson
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.ByteString (ByteString)
import Data.Int (Int64)
import qualified Database.Persist.TH as PTH
import Data.Text (Text)
import Data.Time (UTCTime)

PTH.share [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"] [PTH.persistLowerCase|

  User sql=users
    name Text
    email Text
    deriving Show Eq
    UniqueEmail email

  Event sql=events
    name Text
    startTime UTCTime
    onSaleTime UTCTime
    creatorId UserId
    deriving Show Eq
    UniqueNameCreator name creatorId

  EventTicket sql=event_tickets
    eventId EventId
    tier Text
    price Double
    deriving Show Eq

  PurchaseRecord sql=purchase_records
    eventTicketId EventTicketId
    purchaser UserId
    deriving Show Eq

  AuthData sql=auth_data
    userId UserId
    hashString ByteString
    userRole Text
    UniqueUserId userId
    deriving Show Eq

|]

newtype NormalUserId = NormalUserId Int64
  deriving (Show, Eq)

newtype CreatorUserId = CreatorUserId Int64
  deriving (Show, Eq)

data LoginInfo = LoginInfo
  { loginInfoEmail :: Text
  , loginInfoPassword :: Text
  } deriving (Show, Eq)

data EventSummary = EventSummary
  { event :: Event
  , tiers :: [(Text, Double, Int64, Int64)] -- Tier name, price, sold so far, total
  } deriving (Show, Eq)

deriveJSON defaultOptions ''User
deriveJSON defaultOptions ''Event
deriveJSON defaultOptions ''EventTicket
deriveJSON defaultOptions ''PurchaseRecord
deriveJSON defaultOptions ''LoginInfo
deriveJSON defaultOptions ''EventSummary
