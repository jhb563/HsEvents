{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Backend.Server where

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as Lazy
import Data.Int (Int64)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Database.Persist.Postgresql (ConnectionString, fromSqlKey)
import Network.Wai.Handler.Warp (run)
import Servant.API
import Servant.Server
import Servant.Server.StaticFiles (serveDirectoryWebApp)

import Common.API
import Common.Schema

import Backend.Database
import Backend.HTML
import Backend.Monad.App
import Backend.Monad.Database

------------------------------------------------------------------------------------------
-- User Endpoints
------------------------------------------------------------------------------------------
createUserHandler :: (User, Text) -> ServerMonad Int64
createUserHandler = createUser

loginHandler :: LoginInfo -> ServerMonad (Maybe Int64)
loginHandler info = do
  result <- loginUser info "user"
  case result of
    Right uid -> return (Just uid)
    _ -> return Nothing

usersServer :: ServerT UsersApi ServerMonad
usersServer = createUserHandler :<|> loginHandler
------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------
-- Event Endpoints
------------------------------------------------------------------------------------------
browseEventsHandler :: NormalUserId -> ServerMonad [(Int64, Event)]
browseEventsHandler _ = do
  allEvents <- getCurrentEvents
  return (unKeyVal <$> allEvents) 

purchasedEventsHandler :: NormalUserId -> ServerMonad [(Event, [EventTicket])]
purchasedEventsHandler (NormalUserId uid) = getPurchasedEvents uid

createEventHandler :: CreatorUserId -> EventSummary -> ServerMonad Int64
createEventHandler (CreatorUserId uid) (EventSummary e ts) = do
  eid <- createEvent e
  createTicketsForEvent eid (dropAlreadySold <$> ts)
  return eid

creatorSummaryHandler :: CreatorUserId -> ServerMonad [(Int64, Event)]
creatorSummaryHandler (CreatorUserId uid) = do
  events <- getCreatedEvents uid
  return (unKeyVal <$> events)

creatorEventSummaryHandler :: CreatorUserId -> Int64 -> ServerMonad (Maybe EventSummary)
creatorEventSummaryHandler (CreatorUserId uid) eid = do
  maybeEvent <- fetchEvent eid
  case maybeEvent of
    Nothing -> return Nothing
    Just e -> if fromSqlKey (eventCreatorId e) == uid
      then Just <$> getEventSummary eid
      else return Nothing

deleteEventHandler :: CreatorUserId -> Int64 -> ServerMonad ()
deleteEventHandler (CreatorUserId uid) eid = do
  maybeEvent <- fetchEvent eid
  case maybeEvent of
    Just e -> if fromSqlKey (eventCreatorId e) == uid
      then deleteEvent eid
      else return ()
    _ -> return ()

eventsServer :: ServerT EventsApi ServerMonad
eventsServer =
  browseEventsHandler :<|>
  purchasedEventsHandler :<|>
  createEventHandler :<|>
  creatorSummaryHandler :<|>
  creatorEventSummaryHandler :<|>
  deleteEventHandler
------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------
-- Purchase Endpoints
------------------------------------------------------------------------------------------
enterQueueHandler :: NormalUserId -> Int64 -> ServerMonad ()
enterQueueHandler uid eid = undefined

purchaseTicketHandler :: NormalUserId -> Int64 -> [(Text, Int64)] -> ServerMonad ()
purchaseTicketHandler uid eid purchases = undefined

purchasesServer :: ServerT PurchaseApi ServerMonad
purchasesServer = enterQueueHandler :<|> purchaseTicketHandler
------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------
-- Static Content
------------------------------------------------------------------------------------------
type StaticContentApi =
  "index" :> Get '[HTML] RawHtml :<|>
  Raw

loadIndex :: Handler RawHtml
loadIndex = RawHtml <$>
  (liftIO $ Lazy.readFile "frontend-result/bin/frontend.jsexe/index.html")

staticServer :: Server StaticContentApi
staticServer =
  loadIndex :<|>
  serveDirectoryWebApp "frontend-result/bin/frontend.jsexe"

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
transformServerToHandler :: ConnectionString -> ServerMonad a -> Handler a
transformServerToHandler conn (ServerMonad a) = do
  result <- liftIO $ runSqlAction conn a
  Handler $ return result

fullApiServer :: ConnectionString -> Server FullApi
fullApiServer conn = hoistServerWithContext fullApi authProxy (transformServerToHandler conn) $
  usersServer :<|> eventsServer :<|> purchasesServer

type CompleteServerApi = FullApi :<|> StaticContentApi

completeServerApi :: Proxy CompleteServerApi
completeServerApi = Proxy

completeServerApiServer :: ConnectionString -> Server CompleteServerApi
completeServerApiServer conn = fullApiServer conn :<|> staticServer

------------------------------------------------------------------------------------------
-- Authenication
------------------------------------------------------------------------------------------
normalUserAuthCheck :: ConnectionString -> BasicAuthData -> IO (BasicAuthResult NormalUserId)
normalUserAuthCheck = authCheck NormalUserId "user"

creatorUserAuthCheck :: ConnectionString -> BasicAuthData -> IO (BasicAuthResult CreatorUserId)
creatorUserAuthCheck = authCheck CreatorUserId "creator"

authCheck :: (Int64 -> a) -> Text -> ConnectionString -> BasicAuthData -> IO (BasicAuthResult a)
authCheck userCons role conn (BasicAuthData username password) = do
  let loginInfo = LoginInfo (decodeUtf8 username) (decodeUtf8 password)
  loginResult <- runSqlAction conn (loginUserSql loginInfo role)
  case loginResult of
    Right uid -> return (Authorized (userCons uid))
    Left InvalidPassword -> return BadPassword
    Left _ -> return NoSuchUser

authContext :: ConnectionString ->
  Context (BasicAuthCheck NormalUserId ': BasicAuthCheck CreatorUserId ': '[])
authContext conn =
  BasicAuthCheck (normalUserAuthCheck conn) :.
  BasicAuthCheck (creatorUserAuthCheck conn) :.
  EmptyContext

authProxy :: Proxy (BasicAuthCheck NormalUserId ': BasicAuthCheck CreatorUserId ': '[])
authProxy = Proxy
------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------

runServer :: IO ()
runServer = run 8080
  (serveWithContext
    completeServerApi
    (authContext localConnString)
    (completeServerApiServer localConnString))
