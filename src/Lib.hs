{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Lib
  ( startApp,
    app,
  )
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

data User = User
  { userId :: Int,
    userFirstName :: String,
    userLastName :: String
  }
  deriving (Eq, Show)

data Product = Product
  { productId :: Int,
    productName :: String,
    productDescription :: String
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)
$(deriveJSON defaultOptions ''Product)

type API =
  "users" :> Get '[JSON] [User]
    :<|> "products" :> Get '[JSON] [Product]
    :<|> "postUser" :> ReqBody '[JSON] User :> Post '[JSON] User

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = return users :<|> return products :<|> postUser

users :: [User]
users =
  [ User 1 "Isaac" "Newton",
    User 2 "Albert" "Einstein"
  ]

products :: [Product]
products =
  [ Product 1 "Produto 1" "descrição bala",
    Product 2 "Produto 2" "descrição bala"
  ]

postUser :: User -> Handler User
postUser user = return user