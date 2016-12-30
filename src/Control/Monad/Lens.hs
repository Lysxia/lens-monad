{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Control.Monad.Lens where

import Control.Applicative
import qualified Control.Lens as L
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Functor.Identity
import Data.Profunctor

data LensT m s x a = LensT (GetterT m s x a) (SetterT m s x a)
data SafeLensT m s x a = SafeLensT (GetterT m s x a) (SafeSetterT m s x a)

newtype GetterT m s x a = GetterT (ReaderT s m a)
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus)

newtype SetterT m s x a = SetterT { unSetterT :: x -> StateT s m a }

newtype SafeSetterT m s x a =
  SafeSetterT { unSafeSetterT :: x -> StateT (s, Predicate s) m a }

-- | We keep two different parts to the predicate to avoid
-- making redundant conjunctions.
data Predicate a = Predicate (a -> Bool) (a -> Bool)

satisfy :: Predicate a -> a -> Bool
satisfy (Predicate p1 p2) = liftA2 (&&) p1 p2

(&&+) :: Predicate a -> (a -> Bool) -> Predicate a
Predicate p1 p2 &&+ p3 = Predicate p1 (liftA2 (&&) p2 p3)

predicate :: (a -> Bool) -> Predicate a
predicate p1 = Predicate p1 (pure True)

instance Functor m => Profunctor (GetterT m s) where
  rmap = fmap
  lmap _ (GetterT get) = GetterT get

instance Functor m => Functor (SetterT m s x) where
  fmap f (SetterT set) = SetterT ((fmap . fmap) f set)

instance Functor m => Profunctor (SetterT m s) where
  rmap = fmap
  lmap g (SetterT set) = SetterT (set . g)

instance Monad m => Applicative (SetterT m s x) where
  pure a = SetterT (\_ -> pure a)
  SetterT setf <*> SetterT seta = SetterT (liftA2 (<*>) setf seta)

instance Monad m => Monad (SetterT m s x) where
  SetterT seta >>= toSetterT = SetterT
    (\x -> seta x >>= (`unSetterT` x) . toSetterT)

instance Functor m => Functor (SafeSetterT m s x) where
  fmap f (SafeSetterT set) = SafeSetterT ((fmap . fmap) f set)

instance Functor m => Profunctor (SafeSetterT m s) where
  rmap = fmap
  lmap g (SafeSetterT set) = SafeSetterT (set . g)

instance Monad m => Applicative (SafeSetterT m s x) where
  pure a = SafeSetterT (\_ -> pure a)
  SafeSetterT setf <*> SafeSetterT seta = SafeSetterT (liftA2 (<*>) setf seta)

instance Monad m => Monad (SafeSetterT m s x) where
  SafeSetterT seta >>= toSafeSetterT = SafeSetterT
    (\x -> seta x >>= (`unSafeSetterT` x) . toSafeSetterT)

class Lensey lensT where
  runLens :: lensT Identity s x a -> L.Lens s s a x
  liftLens :: Monad m => L.Lens s s a x -> lensT m s x a
  zoomLens :: Monad m => L.Lens' s t -> lensT m t x a -> lensT m s x a

instance Lensey LensT where
  runLens (LensT (GetterT get_) (SetterT set_)) =
    L.lens (runReader get_) (\s x -> execState (set_ x) s)
  liftLens l = LensT
    (liftGetter l)
    (liftSetter l)
  zoomLens l (LensT (GetterT get) (SetterT set)) = LensT
    (GetterT (L.magnify l get))
    (SetterT (\x -> L.zoom l (set x)))

runSafeLens :: SafeLensT Identity s x a -> L.Lens s s a x
runSafeLens (SafeLensT (GetterT get_) (SafeSetterT set_)) =
  L.lens
    (runReader get_)
    (\s x -> fst (execState (set_ x) (s, predicate (pure True))))

type Getting_ s a x = (a -> L.Const a x) -> (s -> L.Const a s)
-- L.Getting a s a = Getting_ s a a

liftGetter :: Monad m => Getting_ s a x -> GetterT m s x a
liftGetter l = GetterT (L.view (L.getting l))

liftSetter :: Monad m => L.Lens s s a x -> SetterT m s x a
liftSetter l = SetterT $ \x -> L.assign l x >> L.use (L.getting l)

liftSafeSetter
  :: (MonadError () m, Eq a)
  => L.Lens s s a x -> SafeSetterT m s x a
liftSafeSetter l = SafeSetterT $ \x -> do
    (s, p) <- get
    let s' = L.set l x s
        a' = L.view lGet s'
    unless (satisfy p s') $ throwError ()
    put (s', p &&+ \s'' -> L.view lGet s'' == a')
    pure a'
  where
    lGet = L.getting l

liftSafeLens
  :: (MonadError () m, Eq a)
  => L.Lens s s a x -> SafeLensT m s x a
liftSafeLens l = SafeLensT
  (liftGetter l)
  (liftSafeSetter l)

zoomGetter
  :: Monad m
  => L.Lens' s t
  -> GetterT m t x a
  -> GetterT m s x a
zoomGetter l (GetterT get) = GetterT (L.magnify l get)

zoomSetter :: Monad m => L.Lens' s t -> SetterT m t x a -> SetterT m s x a
zoomSetter l (SetterT set) = SetterT $ \x -> L.zoom l (set x)

zoomSafeSetter :: Monad m => L.Lens' s t -> SafeSetterT m t x a -> SafeSetterT m s x a
zoomSafeSetter l (SafeSetterT set) = SafeSetterT $ \x -> L.zoom l' (set x)
  where
    l' f (s0, p) =
      let
        t0 = L.view l s0
        p1_t t = satisfy p (L.set l t s0)
        reset (t, (Predicate _ p2_t)) = (L.set l t s0, p &&+ (p2_t . L.view l))
      in
        reset <$> f (t0, predicate p1_t)

zoomSafeLens
  :: Monad m => L.Lens s s t t -> SafeLensT m t x a -> SafeLensT m s x a
zoomSafeLens l (SafeLensT get set) = SafeLensT
  (zoomGetter l get)
  (zoomSafeSetter l set)

class Observer m where
  observe :: (s -> a) -> m s x a

instance Monad m => Observer (GetterT m) where
  observe = GetterT . reader

instance Monad m => Observer (SetterT m) where
  observe = SetterT . const . gets

safeObserve :: (Monad m, Eq a) => (s -> a) -> SafeSetterT m s x a
safeObserve f = SafeSetterT $ \_ -> do
    (s, p) <- get
    let a = f s
    put (s, p &&+ \s' -> f s' == a)
    pure a
