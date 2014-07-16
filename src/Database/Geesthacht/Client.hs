module Database.Geesthacht.Client
       ( createTable
       )
       where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L
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
