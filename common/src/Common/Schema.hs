{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Common.Schema where

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

|]
