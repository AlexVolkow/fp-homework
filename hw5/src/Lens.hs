{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE Rank2Types #-}

module Lens
       ( Lens
       , Lens'
       , set
       , view
       , over
       , (.~)
       , (^.)
       , (%~)
       , _1
       , _2
       , lens
       , choosing
       , (<%~)
       , (<<%~)
       ) where

import  Data.Functor.Const    (Const (..), getConst)
import  Data.Functor.Identity (Identity (..), runIdentity)

type Lens s t a b = forall f . Functor f => (a -> f b) -> s -> f t
type Lens' s a  = Lens s s a a

set :: Lens' s a -> a -> s -> s
set l b = runIdentity . l (\_ -> Identity b)

view :: Lens' s a -> s -> a
view l s = getConst (l Const s)

over :: Lens' s a -> (a -> a) -> s -> s
over l f = runIdentity . l (Identity . f)

(.~) :: Lens' s a -> a -> s -> s
(.~) = set

(^.) :: s -> Lens' s a -> a
s ^. l = view l s

(%~) :: Lens' s a -> (a -> a) -> s -> s
(%~) = over

-- _1 :: Functor f => (a -> f b) -> (a, x) -> f (b, x)
_1 :: Lens (a, x) (b, x) a b
_1 f (a, x) = (\b -> (b, x)) <$> f a

-- _2 :: Functor f => (a -> f b) -> (x, a) -> f (x, b)
_2 :: Lens (x, a) (x, b) a b
_2 f (x, b) = (\a -> (x, a)) <$> f b

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens getter setter f s = setter s <$> f (getter s)

set' :: Lens s t a b -> b -> s -> t
set' l b = runIdentity . l (\_ -> Identity b)

over' :: Lens s t a b -> (a -> b) -> s -> t
over' l f = runIdentity . l (Identity . f)

view' :: s -> Lens s t a b -> a
view' s l = getConst (l Const s)

choosing :: Lens s1 t1 a b
         -> Lens s2 t2 a b
         -> Lens (Either s1 s2) (Either t1 t2) a b
choosing l1 l2 = lens (either (`view'` l1) (`view'` l2))
                      (either (\s1 b -> Left  $ (set' l1 b) s1) (\s2 b -> Right $ (set' l2 b) s2))

(<%~) :: Lens s t a b -> (a -> b) -> s -> (b, t)
(<%~) l f s = (f (view' s l), (over' l f) s)

(<<%~) :: Lens s t a b -> (a -> b) -> s -> (a, t)
(<<%~) l f s = (view' s l, (over' l f) s)