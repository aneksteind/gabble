{-# LANGUAGE DeriveFunctor #-}

module Recursive (
    cons,
    nil,
    CoAlgebraM,
    AlgebraM,
    Algebra,
    CoAlgebra,
    splitAtF,
    takeF,
    (+++),
    (!!!),
    lengthF,
    sortF
) where

import Data.Functor.Foldable (Fix (..), ListF (..), cata, ana, hylo)

type List a = Fix (ListF a)

cons :: a -> List a -> List a
cons a b = Fix (Cons a b)

nil :: List a
nil = Fix (Nil)

type Algebra f a = f a -> a
type AlgebraM m f a = f a -> m a
type CoAlgebraM m f a = a -> m (f a)
type CoAlgebra f a = a -> f a

-- splitAt equivalent for ListF
splitAtF :: Int -> List a -> (List a, List a)
splitAtF n ls
  | n <= 0 = (nil, ls)
  | otherwise          = splitAt' n ls
    where
        splitAt' _  (Fix Nil)     = (nil, nil)
        splitAt' 1  (Fix (Cons x xs)) = (cons x nil, xs)
        splitAt' m  (Fix (Cons x xs)) = (cons x xs', xs'')
          where
            (xs', xs'') = splitAt' (m - 1) xs

-- take equivalent for ListF
takeF :: Int -> List a -> List a
takeF n xs = ana grab (n,xs) where
    grab :: CoAlgebra (ListF a) (Int, List a)
    grab (_,Fix Nil) = Nil
    grab (0,_) = Nil
    grab (m,Fix (Cons x xs)) = Cons x (m-1,xs) 

-- (!!) equivalent for ListF (unsafe)
(!!!) :: List a -> Int -> a
(Fix (Cons x _)) !!! 0 = x
(Fix (Cons _ xs)) !!! n = xs !!! (n-1)

-- (++) equivalent for ListF
(+++) :: List a -> List a -> List a
(Fix Nil) +++ x = x
x +++ (Fix Nil) = x
(Fix (Cons x xs)) +++ ys = cons x (xs +++ ys)

-- length equivalent for ListF
lengthF :: List a -> Int
lengthF = cata add where
    add Nil = 0
    add (Cons _ s) = 1 + s

addNode :: CoAlgebra (Tree a) (List a)
addNode (Fix Nil) = Empty
addNode (Fix (Cons x (Fix Nil))) = Leaf x
addNode xs = Node l r where
    (l, r) = splitAtF (lengthF xs `div` 2) xs

merge :: Ord a => Algebra (Tree a) (List a)
merge Empty = nil
merge (Leaf x) = cons x nil
merge (Node l r) = mergeF (l,r) where
    mergeF :: Ord a => (List a, List a) -> List a
    mergeF (Fix Nil,ys)  = ys
    mergeF (xs,Fix Nil)  = xs
    mergeF (Fix (Cons x xs), Fix (Cons y ys))
      = case x `compare` y of
         LT -> cons y $ mergeF ((cons x xs), ys)
         _  -> cons x $ mergeF (xs, (cons y ys))

-- merge sort (descending) for ListF
sortF :: Ord a => List a -> List a
sortF = hylo merge addNode

data Tree a r =
    Empty
  | Leaf a
  | Node r r
  deriving Functor