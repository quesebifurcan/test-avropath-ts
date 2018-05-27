{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

import Web.Spock
import Web.Spock.Config

import qualified Data.Set as S
import Data.Text.Prettyprint.Doc

import Data.Aeson hiding (json)
import Data.Monoid
import Data.Text (Text, pack)
import GHC.Generics

import qualified Data.Avro.Types as Ty
import Generic.Random
import Test.QuickCheck
import Data.Avro.Schema as Schema
import AvroPath

import Test.QuickCheck.Instances.Text
import Test.QuickCheck.Instances.UnorderedContainers
import Test.QuickCheck.Instances.Vector
import Test.QuickCheck.Instances.ByteString

import qualified Data.List.NonEmpty as NE

type Api = SpockM () () () ()
type ApiAction a = SpockAction () () () a

data TsResponse = TsResponse { content :: Text }
  deriving (Generic, Show)

instance ToJSON TsResponse
instance FromJSON TsResponse

instance Arbitrary TypeName where
  arbitrary = TN <$> arbitrary

instance Arbitrary Schema.Field where
  arbitrary = genericArbitraryU

instance Arbitrary Order where
  arbitrary = genericArbitraryU

instance Arbitrary (NE.NonEmpty Type) where
  arbitrary = genericArbitrary (1 % ())

instance Arbitrary (Ty.Value Type) where
  arbitrary = genericArbitraryU

-- TODO: "Typed weights"
-- https://hackage.haskell.org/package/generic-random-1.1.0.2/docs/Generic-Random-Tutorial.html
instance Arbitrary Type where
  arbitrary = genericArbitraryU

deriving instance Generic Type
deriving instance Generic (Ty.Value Type)
deriving instance Generic Order
deriving instance Generic Schema.Field

genTs :: Type -> TsResponse
genTs schema =
  TsResponse (pack . show $ content)
  where
    content = vcat $ punctuate line schemas
    schemas = fmap formatTs (S.toList types)
    (TsTypeState types) = snd $ avroToTypescript schema

genRandom count schema = "TODO: random generation of avro records" :: Text

app :: Api
app = do
  post "/random" $ do
    schema <- jsonBody' :: ApiAction Schema
    count <- param' "count"
    json $ genRandom (count :: Int) schema
  post "/" $ do
    schema <- jsonBody' :: ApiAction Schema
    json $ genTs schema

main :: IO ()
main = do
  spockCfg <- defaultSpockCfg () PCNoDatabase ()
  runSpock 8080 (spock spockCfg app)
