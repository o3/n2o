module Web.Nitro.Protocol where

import qualified Data.Map.Strict as M
import qualified Data.ByteString as BS

-- | Nitro protocol message type
data Nitro a
  = NitroInit BS.ByteString
  | NitroPickle { pickleSource :: BS.ByteString
                , picklePickled :: BS.ByteString
                , pickleLinked :: M.Map BS.ByteString BS.ByteString }
  | NitroDone
  deriving (Show)
