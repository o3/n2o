module Network.N2O.Protocols.Types where

import qualified Data.Map.Strict as M
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as BS

data N2OProto a = N2ONitro (Nitro a) | N2OClient (Client a) deriving (Show)

data Client a = C a | S a deriving (Show)

data Nitro a =
   I L.ByteString
 | P { pickleSource  :: L.ByteString
     , picklePickled :: L.ByteString
     , pickleLinked :: M.Map BS.ByteString L.ByteString
     }
 deriving (Show)
