{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}

module Network.N2O.Generic where

import           Data.Proxy
import           GHC.Generics

class Names' rep where
  names' :: Proxy rep -> [String]
instance (Names' f) => Names' (M1 D t f) where
  names' _ = names' (Proxy @f)
instance (Names' f, Names' g) => Names' (f :+: g) where
  names' _ = (names' (Proxy @f) ++ (names' (Proxy @g)))
instance (Constructor c) => Names' (C1 c f) where
  names' _ = [conName (undefined :: C1 c f g)]

