module Data.SkewList where

import Data.List as L
import Data.List (List)
import Prelude
import Data.Newtype
import Data.Maybe
import Data.Generic.Rep.Show
import Data.Generic.Rep (class Generic)
import Data.Foldable

data Tree a = Leaf a | Node a (Tree a) (Tree a)

derive instance genericTree :: Generic (Tree a) _

instance showTree :: Show a => Show (Tree a) where
  show = genericShow

newtype SkewList a = SkewList (List { weight :: Int, tree :: Tree a })

derive instance newtypeSkewList :: Newtype (SkewList a) _
derive instance genericSkewList :: Generic (SkewList a) _

instance showSkewList :: Show a => Show (SkewList a) where
  show = genericShow

nil = SkewList mempty

cons :: forall a. a -> SkewList a -> SkewList a
cons x (SkewList ts@({ weight: w₁, tree: t₁ } L.:
                     { weight: w₂, tree: t₂ } L.: rest))
  | w₁ == w₂ =
    SkewList $ { weight: 1 + w₁ + w₂, tree: Node x t₁ t₂ } L.: rest
cons x (SkewList ts) =
  SkewList $ { weight: 1, tree: Leaf x } L.: ts


uncons :: forall a. SkewList a -> Maybe { head :: a, tail :: SkewList a }
uncons (SkewList ({ weight: 1, tree: Leaf x } L.: ts)) =
  Just { head: x, tail: SkewList ts }
uncons (SkewList ({ weight, tree: Node x t₁ t₂ } L.: ts )) =
  Just { head: x
       , tail: SkewList $
         { weight: weight `div` 2
         , tree: t₁ } L.:
         { weight: weight `div` 2
         , tree: t₂ } L.:
         ts
       }
uncons _ = Nothing


-- fixed version
lookup :: forall a. Int -> SkewList a -> Maybe a
lookup ix (SkewList ({ weight, tree } L.: ts)) =
  if ix < weight
  then lookupTree weight tree ix
  else lookup (ix - weight) (SkewList ts)
lookup _ _ = Nothing

-- fixed version
lookupTree :: forall a. Int -> Tree a -> Int -> Maybe a
lookupTree w (Node x _ _) 0 = Just x
lookupTree w (Node x t₁ t₂) ix =
  if ix <= w `div` 2
  then lookupTree (w `div` 2) t₁ (ix - 1)
  else lookupTree (w `div` 2) t₂ (ix - 1 - w `div` 2)
lookupTree 1 (Leaf x) 0 = Just x
lookupTree _ (Leaf x) _ = Nothing


-- original version from PFDS
lookupOriginal :: forall a. Int -> SkewList a -> Maybe a
lookupOriginal ix (SkewList ({ weight, tree } L.: ts)) =
  if ix < weight
  then lookupTreeOriginal weight tree ix
  else lookupOriginal (ix - weight) (SkewList ts)
lookupOriginal _ _ = Nothing

-- original version from PFDS
lookupTreeOriginal :: forall a. Int -> Tree a -> Int -> Maybe a
lookupTreeOriginal w (Node x _ _) 0 = Just x
lookupTreeOriginal w (Node x t₁ t₂) ix =
  if ix < w `div` 2
  then lookupTreeOriginal (w `div` 2) t₁ (ix - 1)
  else lookupTreeOriginal (w `div` 2) t₂ (ix - 1 - w `div` 2)
lookupTreeOriginal 1 (Leaf x) 0 = Just x
lookupTreeOriginal _ (Leaf x) _ = Nothing
