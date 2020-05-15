{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}

module Control.Monad.Lens.Simple
  ( L()
  , lensE
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
  get :: s -> Either [String] v,
  put :: s -> u -> (s, Predicate s, v) }
  deriving Functor

type Predicate s = s -> Bool

map3 :: (c -> c') -> (a, b, c) -> (a, b, c')
map3 f (a, b, c) = (a, b, f c)

instance Profunctor (L s) where
  dimap f g l = L (fmap g . get l) (\s -> map3 g . put l s . f)

instance Applicative (L s u) where
  pure = return
  (<*>) = ap

instance Monad (L s u) where
  return y = L (\ _ -> Right y) (\ s _ -> (s, \_ -> True, y))

  ly >>= kz = L getter putter
    where
      getter s = do
        a <- get ly s
        get (kz a) s
      putter s x = let (s', vy, y) = put ly s x
                       (s'', vz, z) = put (kz y) s' x
                   in (s'', liftA2 (&&) vy vz, z)

lensE :: (s -> Either [String] v) -> (s -> v -> s) -> Lens s s (Either [String] v) (Either [String] v)
lensE get put = Lens.lens get put'
  where
    put' s (Right a) = put s a
    put' s _ = s

-- | Conversion from standard 'Lens' to 'L'.
toL :: Eq v => Lens s s (Either [String] v) (Either [String] v) -> L s v v
toL l = L getter putter
  where
    getter = Lens.view l
    putter s x =
      let s' = Lens.set l (Right x) s
      in (s', \s' -> Lens.view l s' == Right x, x)

-- | Conversion from 'L' to standard 'Lens'.
fromL :: L s u v -> Lens s s (Either [String] v) (Either [String] u)
fromL l = Lens.lens (get l) $ \s x -> case x of
  Right x' -> let (s', validate, _) = put l s x' in if validate s' then s' else error "Put conflict"
  Left _ -> s
