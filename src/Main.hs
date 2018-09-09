{-# language Rank2Types    #-}
{-# language TupleSections #-}
module Main where

import Data.Complex
import Data.Functor.Const
import Data.Functor.Identity

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

type Simple f s a = f s s a a
type Lens' s a = Simple Lens s a

view :: Lens' s a -> s -> a
view l s = getConst (l Const s)

over :: Lens' s a -> (a -> a) -> s -> s
over l f = runIdentity . l (Identity . f)

set :: Lens' s a -> a -> s -> s
set l = over l . const

realLens :: RealFloat a => Lens' (Complex a) a
realLens f (r :+ i) = fmap (:+ i) (f r)

imagLens :: RealFloat a => Lens' (Complex a) a
imagLens f (r :+ i) = fmap (r :+) (f i)

_1 :: Lens (a, b) (a', b) a a'
_1 f (a, b) = (,b) <$> f a

_2 :: Lens (a, b) (a, b') b b'
_2 f (a, b) = (a,) <$> f b

main :: IO ()
main = do
  print $ set realLens (1 :: Double) (0 :+ 1)
  print $ view _1 ('x', 'y')
