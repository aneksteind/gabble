{-# LANGUAGE DeriveFunctor #-}

module Recursive (
    CoAlgebraM,
    AlgebraM,
    Algebra
) where

type Algebra f a = f a -> a
type AlgebraM m f a = f a -> m a
type CoAlgebraM m f a = a -> m (f a)
type CoAlgebra f a = a -> f a