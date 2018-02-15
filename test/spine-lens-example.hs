{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE OverloadedStrings #-}

import Control.Lens
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Gen.QuickCheck as Gen
import Test.QuickCheck.Hedgehog
import Test.QuickCheck (Arbitrary(..), CoArbitrary(..), Function(..))

import Spine

instance Arbitrary Tree where
  arbitrary = hedgehog genTree
  shrink Leaf = []
  shrink (Node l a r) =
    [l, r] ++
    [Node l' a r | l' <- shrink l] ++
    [Node l a' r | a' <- shrink a] ++
    [Node l a r' | r' <- shrink r]

instance CoArbitrary Tree
instance Function Tree

genTree :: Gen Tree
genTree = Gen.sized genTree' 

genTree' :: Size -> Gen Tree
genTree' 0 = return Leaf
genTree' n = Gen.choice
  [ return Leaf
  , Node <$> genTree' (n-1) <*> Gen.arbitrary <*> genTree' (n-1)
  ]

genNode :: Gen Tree
genNode = Node <$> genTree <*> Gen.arbitrary <*> genTree

isLens :: (Eq a, Show a, Eq s, Show s, MonadTest m) => ALens' s a -> s -> a -> m ()
isLens lens s a = do
  let get = view (cloneLens lens)
      put = set (cloneLens lens)
  put (get s) s === s
  get (put a s) === a

main :: IO Bool
main = checkParallel $ Group "spine-lens-example"
  [ ("rootL",  property $ do
      t <- forAll genTree
      a <- forAll Gen.arbitrary
      isLens rootL t a)
  , ("rightL", property $ do
      t <- forAll genNode
      r <- forAll genTree
      isLens rightL t r)
  , ("spineL", property $ do
      t <- forAll genTree
      as <- forAll Gen.arbitrary
      isLens spineL t as)
  ]
