module Database.Geesthacht.Types
       ( Host
       , Method
       , Reply
       )
       where

import qualified Data.ByteString.Lazy.Char8 as L
import Network.HTTP.Client
import qualified Network.HTTP.Types.Method as NHTM

type Reply = Network.HTTP.Client.Response L.ByteString
type Method = NHTM.Method

newtype Host = Host String deriving (Eq, Show)
