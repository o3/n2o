module Network.N2O.Protocols.Types where

import qualified Data.Map.Strict as M
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as BS

newtype N2OProto a = N2ONitro (Nitro a) deriving (Show)

data Nitro a =
   I L.ByteString
 | P { pickleSource  :: L.ByteString
     , picklePickled :: L.ByteString
     , pickleLinked :: M.Map BS.ByteString L.ByteString
     }
 deriving (Show)
