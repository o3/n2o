{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Network.N2O.Generic where

import           Data.Proxy
import           GHC.Generics

-- TODO: get constructor names, use Data.Proxy see
-- https://stackoverflow.com/questions/27815489/is-it-possible-to-list-the-names-and-types-of-fields-in-a-record-data-type-that

class Names' rep where
  names' :: Proxy rep -> [String]
instance (Names' f) => Names' (M1 D t f) where
  names' _ = names' (Proxy @f)
instance (Names' f, Names' g) => Names' (f :+: g) where
  names' _ = (names' (Proxy @f) ++ (names' (Proxy @g)))
instance (Constructor c) => Names' (C1 c f) where
  names' _ = [conName (undefined :: C1 c f g)]

class GetName' f where
  getName' :: f p -> String
instance (GetName' f) => GetName' (M1 D t f) where
  getName' = getName' . unM1
instance (GetName' f, GetName' g) => GetName' (f :+: g) where
  getName' (L1 l) = getName' l
  getName' (R1 r) = getName' r
instance (Constructor t) => GetName' (C1 t f) where
  getName' = conName

getName :: (Generic a, GetName' (Rep a)) => a -> String
getName = getName' . from
class GetIndex' f where
  getIndex' :: f p -> Int
  size :: Proxy f -> Int
instance (GetIndex' f) => GetIndex' (M1 i t f) where
  getIndex' (M1 x) = getIndex' x
  size _ = size (Proxy @f)
instance GetIndex' V1 where
  getIndex' _ = undefined
  size _ = undefined
instance (GetIndex' f, GetIndex' g) => GetIndex' (f :+: g) where
  getIndex' (L1 x) = getIndex' x
  getIndex' (R1 x) = size (Proxy @f) + getIndex' x
  size _ = size (Proxy @f) + size (Proxy @g)
instance GetIndex' (f :*: g) where
  getIndex' _ = 0
  size _ = 1
instance GetIndex' (K1 i c) where
  getIndex' _ = 0
  size _ = 1
instance GetIndex' U1 where
  getIndex' _ = 0
  size _ = 1

getIndex :: (Generic a, GetIndex' (Rep a)) => a -> Int
getIndex = getIndex' . from

data Test = Foo | Bar Int | Baz { x :: Int } deriving (Generic, Show, Eq)
main = do
  print $ getIndex Foo
  print $ getIndex $ Bar undefined
  print $ getIndex $ Baz {x = undefined}
  print $ getName $ Baz {x = undefined}
  print $ names' (Proxy :: Proxy (Rep Test))
