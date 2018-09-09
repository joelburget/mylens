{-# language FlexibleInstances      #-}
{-# language FunctionalDependencies #-}
{-# language MultiParamTypeClasses  #-}
{-# language Rank2Types             #-}
{-# language TupleSections          #-}
module Main where

import Data.Complex
import Data.Functor.Const
import Data.Functor.Identity
import Data.Profunctor

import Data.Profunctor.Unsafe ((#.))

type Lens s t a b = forall   f. Functor f                 => (a -> f b) -> s -> f t

-- Generalize `->` to `(Profunctor p =>) p`.
type Iso  s t a b = forall p f. (Functor f, Profunctor p) => p a (f b)  -> p s (f t)

type Simple f s a = f s s a a
type Lens' s a = Simple Lens s a
type Iso'  s a = Simple Iso  s a

iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso sa bt = dimap sa (fmap bt)

data Exchange a b s t = Exchange (s -> a) (b -> t)

instance Functor (Exchange a b s) where
  fmap g' (Exchange f g) = Exchange f (g' . g)

instance Profunctor (Exchange a b) where
  dimap f' g' (Exchange f g) = Exchange (f . f') (g' . g)

withIso :: Iso s t a b -> ((s -> a) -> (b -> t) -> r) -> r
withIso ai k = case ai (Exchange id Identity) of
  Exchange sa bt -> k sa (runIdentity #. bt)

from :: Iso s t a b -> Iso b a t s
from l = withIso l $ \ sa bt -> iso bt sa

view :: Lens s t a b -> s -> a
view l s = getConst (l Const s)

over :: Lens s t a b -> (a -> b) -> s -> t
over l f = runIdentity . l (Identity . f)

set :: Lens s t a b -> b -> s -> t
set l = over l . const

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens get' set' f s = set' s <$> f (get' s)

realLens, realLens' :: RealFloat a => Lens' (Complex a) a
realLens f (r :+ i) = fmap (:+ i) (f r)

imagLens, imagLens' :: RealFloat a => Lens' (Complex a) a
imagLens f (r :+ i) = fmap (r :+) (f i)

realLens' = lens (\(r :+ _) -> r) (\(_ :+ i) r -> r :+ i)
imagLens' = lens (\(_ :+ i) -> i) (\(r :+ _) i -> r :+ i)

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap

class Field1 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _1 :: Lens s t a b

instance Field1 (a, b) (a', b) a a' where
  _1 f ~(a, b) = f a <&> \a' -> (a', b)

instance Field1 (a, b, c) (a', b, c) a a' where
  _1 k ~(a, b, c) = k a <&> \a' -> (a', b, c)

class Field2 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _2 :: Lens s t a b

instance Field2 (a, b) (a, b') b b' where
  _2 f ~(a, b) = f b <&> \b' -> (a, b')

instance Field2 (a, b, c) (a, b', c) b b' where
  _2 k ~(a, b, c) = k b <&> \b' -> (a, b', c)

main :: IO ()
main = do
  print $ set realLens (1 :: Double) (0 :+ 1)
  print $ view _1 ('x', 'y', 'z')
  print $ view _2 $ set _2 False ('x', 'y', 'z')
