{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Web.Spock
import Web.Spock.Config

import Data.Aeson hiding (json)
import Data.Monoid
import Data.Text (Text, pack)
import GHC.Generics

-- import AvroPath (getTsTypeName, TsType(..))

-- TODO: use avro types from lib?
type Api = SpockM () () () ()

type ApiAction a = SpockAction () () () a

data Test = Test { a :: Int }
  deriving (Generic, Show)

instance ToJSON Test
instance FromJSON Test

main :: IO ()
main = do
  spockCfg <- defaultSpockCfg () PCNoDatabase ()
  runSpock 8080 (spock spockCfg app)

app :: Api
app = do
  post "/" $ do
    test <- jsonBody' :: ApiAction Test
    json (result test)
    where
      result x = Test { a = (a x) + 1 }


