{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}

module Control.Monad.Lens.Simple
  ( L()
  , (>>=!)
  , toL
  , fromL
  ) where

import Control.Lens (Lens)
import qualified Control.Lens as Lens
import Control.Monad
import Data.Profunctor

-- | Monadic lenses.
data L s u v = L {
  get :: s -> v,
  put :: s -> u -> (s, v) }
  deriving Functor

instance Profunctor (L s) where
  dimap f g l = L (g . get l) (\s -> fmap g . put l s . f)

instance Applicative (L s u) where
  pure = return
  (<*>) = ap

instance Monad (L s u) where
  return y = L (\ _ -> y) (\ s _ -> (s, y))

  ly >>= kz = L getter putter
    where
      getter s = get (kz (get ly s)) s
      putter s x = let (s', y) = put ly s x in put (kz y) s' x

-- | A variant of @('>>=')@ which detects put-conflicts.
(>>=!) :: Eq v => L s u v -> (v -> L s u w) -> L s u w
ly >>=! kz = L getter putter
  where
    getter s = get (kz (get ly s)) s
    putter s x =
      let (s', y) = put ly s x
          (s'', z) = put (kz y) s' x
      in if get ly s'' == y then
        (s'', z)
      else
        error "Put conflict!"

-- | Conversion from standard 'Lens' to 'L'.
toL :: Lens s s v u -> L s u v
toL l = L getter putter
  where
    getter = Lens.view (Lens.getting l)
    putter s x =
      let (v, s') = (l Lens.%%~ \y -> (y, x)) s
      in (s', v)

-- | Conversion from 'L' to standard 'Lens'.
fromL :: L s u v -> Lens s s v u
fromL l = Lens.lens (get l) (\s x -> fst (put l s x))
