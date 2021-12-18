{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( startApp )
where

import Control.Concurrent
import Control.Exception (bracket)
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.TH
import Data.ByteString (ByteString)
import Data.Pool
import Database.PostgreSQL.Simple
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.Client
import Database.PostgreSQL.Simple.FromRow

localPg :: ConnectInfo 
localPg = defaultConnectInfo 
        {
          connectHost     = "localhost",
          connectDatabase = "postgres",
          connectUser     = "postgres",
          connectPassword = "postgres"
        }

data User = User
  { 
    id :: Int,
    name :: String
  }
  deriving (Eq, Show)

instance FromRow User where
  fromRow = User <$> field <*> field

-- data Product = Product
--   { 
--     productId :: Int,
--     productName :: String,
--     productDescription :: String
--   }
--   deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)
-- $(deriveJSON defaultOptions ''Product)

type API = "getUsers" :> Get '[JSON] [User]

-- type API =
--   "getUsers" :> Get '[JSON] [User]
--     :<|> "products" :> Get '[JSON] [Product]
--     :<|> "postUser" :> ReqBody '[JSON] User :> Post '[JSON] User

startApp :: IO ()
startApp = do
  conn <- connect localPg
  run 8080 $ serve api $ server conn

api :: Proxy API
api = Proxy

server :: Connection -> Server API
server = getUsers

getUsers :: Connection -> Handler [User]
getUsers conn = liftIO $ query conn "SELECT * FROM \"user\"" ()

-- users :: [User]
-- users =
--   [ User 1 "Isaac" "Newton",
--     User 2 "Albert" "Einstein"
--   ]

-- products :: [Product]
-- products =
--   [ Product 1 "Produto 1" "descrição bala",
--     Product 2 "Produto 2" "descrição bala"
--   ]

-- postUser :: User -> Handler User
-- postUser = pure