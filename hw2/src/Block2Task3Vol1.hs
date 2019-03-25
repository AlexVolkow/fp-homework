{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE UndecidableInstances #-}

module Block2Task3Vol1 where

import Prelude (id)
import Block2Task3(Monad(..), MonadFish(..), MonadJoin(..))

instance Monad m => MonadFish m where
  returnFish = return
  (>=>) f g = \x -> f x >>= g

instance Monad m => MonadJoin m where
  returnJoin = return
  join x = x >>= id

