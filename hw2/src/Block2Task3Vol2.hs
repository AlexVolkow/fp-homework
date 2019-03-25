{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE UndecidableInstances #-}

module Block2Task3Vol2 where

import Prelude (id)
import Block2Task3(Monad(..), MonadFish(..), MonadJoin(..))

instance MonadFish m => MonadJoin m where
  returnJoin = returnFish
  join x = (id >=> id) x

instance MonadFish m => Monad m where
  return = returnFish
  (>>=) x f = (id >=> f) x
