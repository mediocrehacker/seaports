{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module App where

import           Control.Monad.Trans.Except
import           Data.Aeson
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           System.IO

-- * api

type SeaportApi =
  "seaports" :> Get '[JSON] [Seaport] :<|>
  "seaports" :> Capture "seaportId" Integer :> Get '[JSON] Seaport

seaportApi :: Proxy SeaportApi
seaportApi = Proxy

-- * app

run :: IO ()
run = do
  let port = 3000
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
        defaultSettings
  runSettings settings =<< mkApp

mkApp :: IO Application
mkApp = return $ serve seaportApi server

server :: Server SeaportApi
server =
  getSeaports :<|>
  getSeaportById

getSeaports :: Handler [Seaport]
getSeaports = return [exampleSeaport]

getSeaportById :: Integer -> Handler Seaport
getSeaportById = \ case
  0 -> return exampleSeaport
  _ -> throwE err404

exampleSeaport :: Seaport
exampleSeaport = Seaport 0 "example seaport"

-- * Seaport

data Seaport
  = Seaport {
    seaportId :: Integer,
    seaportText :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON Seaport
instance FromJSON Seaport

data a + b = Foo a b

type X = Int + Bool
