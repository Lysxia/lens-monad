{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}

module Control.Monad.Lens.Simple
  ( L()
  , toL
  , fromL
  ) where

import Control.Applicative (liftA2)
import Control.Lens (Lens)
import qualified Control.Lens as Lens
import Control.Monad
import Data.Profunctor

-- | Monadic lenses.
data L s u v = L {
  get :: s -> v,
  put :: s -> u -> (s, Predicate s, v) }
  deriving Functor

type Predicate s = s -> Bool

map3 :: (c -> c') -> (a, b, c) -> (a, b, c')
map3 f (a, b, c) = (a, b, f c)

instance Profunctor (L s) where
  dimap f g l = L (g . get l) (\s -> map3 g . put l s . f)

instance Applicative (L s u) where
  pure = return
  (<*>) = ap

instance Monad (L s u) where
  return y = L (\ _ -> y) (\ s _ -> (s, \ s -> True, y))

  ly >>= kz = L getter putter
    where
      getter s = get (kz (get ly s)) s
      putter s x = let (s', vy, y) = put ly s x
                       (s'', vz, z) = put (kz y) s' x
                   in (s'', liftA2 (&&) vy vz, z)

-- | Conversion from standard 'Lens' to 'L'.
toL :: Eq v => Lens s s v v -> L s v v
toL l = L getter putter
  where
    getter = Lens.view l
    putter s x =
      let s' = Lens.set l x s
      in (s', \s' -> Lens.view l s' == x, x)

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

-- | Conversion from 'L' to standard 'Lens'.
fromL :: L s u v -> Lens s s v u
fromL l = Lens.lens (get l) (\s x ->
  let (s', validate, _) = put l s x in
  if validate s' then s' else error "Put conflict")
