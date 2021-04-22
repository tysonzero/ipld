module Data.IPLD
    ( Node(..)
    ) where

import Data.ByteString (ByteString)
import Data.IPLD.CID (CID)
import Data.Map (Map)
import Data.Text (Text)
import Data.Vector (Vector)

data Node
    = Null
    | Boolean Bool
    | Integer Integer
    | Float Double
    | String Text
    | Bytes ByteString
    | List (Vector Node)
    | Map (Map Text Node)
    | Link CID
    deriving (Eq, Show, Read)
