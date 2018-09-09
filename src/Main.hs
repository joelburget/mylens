{-# language Rank2Types #-}
module Main where

import Data.Complex
import Data.Functor.Const
import Data.Functor.Identity

type Lens a b = forall f. Functor f => (b -> f b) -> a -> f a

view :: Lens a b -> a -> b
view l s = getConst (l Const s)

over :: Lens a b -> (b -> b) -> a -> a
over l f = runIdentity . l (Identity . f)

set :: Lens a b -> b -> a -> a
set l = over l . const

realLens :: RealFloat a => Lens (Complex a) a
realLens f (r :+ i) = fmap (:+ i) (f r)

imagLens :: RealFloat a => Lens (Complex a) a
imagLens f (r :+ i) = fmap (r :+) (f i)

main :: IO ()
main = do
  print $ set realLens 1 (0 :+ 1)
