{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Lib (startApp) where

import Control.Concurrent
import Control.Exception (bracket, handleJust, throwIO, try)
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.TH
import Data.ByteString (ByteString)
import Data.Pool
import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Errors
import Database.PostgreSQL.Simple.FromRow
import GHC.Exception
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Servant.Errors (errorMw)
import Servant
import Servant.Client

localPg :: ConnectInfo
localPg =
  defaultConnectInfo
    { connectHost = "localhost",
      connectDatabase = "postgres",
      connectUser = "postgres",
      connectPassword = "postgres"
    }

data User = User
  { id :: Int,
    name :: String
  }
  deriving (Eq, Show)

instance FromRow User where
  fromRow = User <$> field <*> field

newtype UserUpdate = UserUpdate { newName :: String } deriving(Eq, Show)

data Product = Product
  { productId :: Int,
    productName :: String,
    description :: String,
    userId :: Int
  }
  deriving (Eq, Show)

instance FromRow Product where
  fromRow = Product <$> field <*> field <*> field <*> field

newtype Message = Message {message :: String} deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)
$(deriveJSON defaultOptions ''UserUpdate)
$(deriveJSON defaultOptions ''Product)
$(deriveJSON defaultOptions ''Message)

type API =
  "getUsers" :> Get '[JSON] [User]
    :<|> "createUser" :> ReqBody '[JSON] User :> Post '[JSON] Message
    :<|> "updateUser" :> QueryParam "id" Int :> ReqBody '[JSON] UserUpdate :> Put '[JSON] Message

startApp :: IO ()
startApp = do
  conn <- connect localPg
  run 8080 $ errorMw @JSON @["error", "status"] $ serve api $ server conn

api :: Proxy API
api = Proxy

server :: Connection -> Server API
server conn = getUsers conn :<|> createUser conn :<|> updateUser conn

getUsers :: Connection -> Handler [User]
getUsers conn = liftIO $ query conn "SELECT * FROM \"user\"" ()

createUser :: Connection -> User -> Handler Message
createUser conn (User id name) = do
  query <-
    liftIO $
      catchViolation catcher $
        execute conn "INSERT INTO \"user\" (id, name) VALUES (?, ?)" (id, name)
          >> successMessage "User created with success!"
  case query of
    Right x -> pure x
    Left _ -> throwError $ err400 {errBody = "Bad Request :("}

updateUser :: Connection -> Maybe Int -> UserUpdate -> Handler Message
updateUser conn id (UserUpdate name) = do
  query <-
    liftIO $
      catchViolation catcher $
        execute conn "UPDATE \"user\" SET name = ? WHERE id = ?" (name, id)
          >> successMessage "User updated with success!"
  case query of
    Right x -> pure x
    Left _ -> throwError $ err400 {errBody = "Bad Request :("}

catcher :: forall e b. Exception e => e -> ConstraintViolation -> IO (Either ByteString b)
catcher _ (UniqueViolation e) = pure $ Left e
catcher e _ = throwIO e

successMessage :: String -> IO (Either ByteString Message)
successMessage s = pure $ Right $ Message s