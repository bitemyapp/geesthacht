{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Database.Geesthacht.Client
       ( createTable
       )
       where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L
import Data.List.NonEmpty
import Data.Maybe
import Data.Time
import Data.Time.ISO8601
import Network.HTTP.Client
import qualified Network.HTTP.Types.Method as NHTM
import qualified Network.HTTP.Types.Status as NHTS

import Database.Geesthacht.Types

emptyBody :: L.ByteString
emptyBody = L.pack ""

dispatch :: Method -> String -> Maybe L.ByteString
            -> IO Reply
dispatch dMethod url body = do
  initReq <- parseUrl url
  let reqBody = RequestBodyLBS $ fromMaybe emptyBody body
  let req = initReq { method = dMethod
                    , requestBody = reqBody
                    , checkStatus = \_ _ _ -> Nothing}
  withManager defaultManagerSettings $ httpLbs req

data AWSRequest =
  AWSRequest { host          :: Host
             , requestDate   :: UTCTime
             , authorization :: AWSAuthorization
             , apiTarget     :: AWSTarget }
  deriving (Eq, Show)

data AWSAuthorization =
  -- I don't fucking know
  AWSAuthorization { bleh :: Int }
  deriving (Eq, Show)

-- DynamoDB_20120810.CreateTable
data AWSTarget =
  AWSTarget { dynamoDBAPI :: DynamoDBAPI
            , apiOperation :: APIOperation }
  deriving (Eq, Show)

-- DynamoDB_20120810
data DynamoDBAPI =
  DynamoDBAPIv2
  deriving (Eq, Show)

-- CreateTable
data APIOperation =
  CreateTableOperation
  | DeleteTableOperation
  deriving (Eq, Show)

createTable :: a -> b
createTable = undefined

--TODO: see if we can tie the attr definitions to the other fields
data TableDefinition =
  TableDefinition { tdAttrDefinitions :: AttrDefinitions
                  , tdKeySchema :: KeySchema
                  , tdProvisionedThroughput :: ProvisionedThroughput
                  , tdTableName :: TableName
                    --FIXME: max 5
                  , tdGlobalSecondaryIndexes :: [GlobalSecondaryIndex]
                  , tdLocalSecondaryIndexes :: [LocalSecondaryIndex]} deriving (Eq, Show)

data GlobalSecondaryIndex = GlobalSecondaryIndex IndexDetails ProvisionedThroughput deriving (Eq, Show)
data LocalSecondaryIndex = LocalSecondaryIndex IndexDetails deriving (Eq, Show)

data IndexDetails =
  IndexDetails { indexName :: IndexName
                 --TODO: max of 2
               , indexKeySchema :: KeySchema
               , indexProjection :: Projection
               , indexProvisionedThroughput :: ProvisionedThroughput } deriving (Eq, Show)

newtype IndexName =
  IndexName String deriving (Eq, Show)

--TODO: min 1, max 20 if specified
data Projection = Projection { nonKeyAttributes :: Maybe (NonEmpty AttrDefinition)
                             , projectionType :: Maybe ProjectionType } deriving (Eq, Show)

data ProjectionType =
  KeysOnly
  | Include
  | All deriving (Eq, Show)

newtype AttrDefinitions =
  AttrDefinitions (NonEmpty AttrDefinition) deriving (Eq, Show)

data AttrDefinition =
  AttrDefinition AttrName AttrType deriving (Eq, Show)

newtype AttrName =
  AttrName String deriving (Eq, Show)

data AttrType =
  StringAttr
  | BinaryAttr
  | NumberAttr deriving (Eq, Show)

newtype KeySchema =
  KeySchema (NonEmpty KeySchemaElement) deriving (Eq, Show)

data KeySchemaElement =
  KeySchemaElement AttrName KeyType deriving (Eq, Show)

data KeyType =
  Hash
  | Range deriving (Eq, Show)

data ProvisionedThroughput =
  ProvisionedThroughput { readCapacityUnits :: ReadCapacityUnits
                        , writeCapacityUnits :: WriteCapacityUnits } deriving (Eq, Show)

newtype ReadCapacityUnits = ReadCapacityUnits Int deriving (Eq, Show)

-- not sure if we should encode maximums, they vary on region/context
-- and can be lifted with a phone call as implied by
-- http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Limits.html
mkReadCapacityUnits :: Int -> Maybe ReadCapacityUnits
mkReadCapacityUnits x
  | x > 0     = Just $ ReadCapacityUnits x
  | otherwise = Nothing

newtype WriteCapacityUnits = WriteCapacityUnits Int deriving (Eq, Show)

mkWriteCapacityUnits :: Int -> Maybe WriteCapacityUnits
mkWriteCapacityUnits x
  | x > 0     = Just $ WriteCapacityUnits x
  | otherwise = Nothing

newtype TableName =
  TableName String deriving (Eq, Show)
