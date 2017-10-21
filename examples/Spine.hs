module Spine where

import Control.Lens
import Control.Monad.Lens.Simple
import Data.Profunctor

data Tree
  = Leaf
  | Node Tree Int Tree
  deriving Show

rootL :: Lens' Tree (Maybe Int)
rootL = lens getter setter
  where
    getter Leaf = Nothing
    getter (Node _ n _) = Just n
    setter _ Nothing = Leaf
    setter Leaf (Just n) = Node Leaf n Leaf
    setter (Node l _ r) (Just n) = Node l n r

rightL :: Lens' Tree Tree
rightL = lens (\(Node _ _ r) -> r) (\(Node l n _) r -> Node l n r)

headM :: [a] -> Maybe a
headM [] = Nothing
headM (a : _) = Just a

spineL :: Lens' Tree [Int]
spineL = fromL $ do
  hd <- lmap headM (toL rootL)
  case hd of
    Nothing -> return []
    Just n -> do
      tl <- lmap tail (toL (rightL . spineL))
      return (n : tl)

t0 = Node (Node Leaf 0 Leaf) 1 (Node Leaf 2 Leaf)

-- | [1, 2]
s0 = view spineL t0

-- | Node (Node Leaf 0 Leaf) 3 (Node Leaf 4 (Node Leaf 5 Leaf))
t1 = set spineL [3, 4, 5] t0
