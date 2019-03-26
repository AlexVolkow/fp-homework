{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE UndecidableInstances #-}

module Block2Task3Vol1 where

import Block2Task3 (Monad (..), MonadFish (..), MonadJoin (..))
import Prelude (id)

instance Monad m => MonadFish m where
  returnFish = return
  (>=>) f g = \x -> f x >>= g

instance Monad m => MonadJoin m where
  returnJoin = return
  join x = x >>= id


