module Network.N2O.Protocols.Types where

import qualified Data.Map.Strict as M
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as BS

-- | Top level sum of protocols
data N2OProto a = N2ONitro (Nitro a) | N2OClient (Client a) deriving (Show)

-- | Client protocol message type
data Client a = Cli a | Srv a deriving (Show)

-- | Nitro protocol message type
data Nitro a =
   Init   L.ByteString
 | Pickle { pickleSource  :: L.ByteString
          , picklePickled :: L.ByteString
          , pickleLinked :: M.Map BS.ByteString L.ByteString
          }
 deriving (Show)
