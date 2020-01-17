{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Backend.Server where

import Data.Int (Int64)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Database.Persist.Postgresql (ConnectionString)
import Servant.API
import Servant.Server

import Common.API
import Common.Schema

import Backend.Database
import Backend.Monad.App
import Backend.Monad.Database

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

runServer :: IO ()
runServer = putStrLn "Running Server!"
