{-# LANGUAGE UnicodeSyntax #-}

module Import 
    ( module Control.Applicative
    , module Control.Applicative.Unicode
    , module Control.Monad.Unicode
    , module Data.Monoid.Unicode
    , module Prelude.Unicode
    , (◁)
    , (▷)
    ) where

import Control.Applicative ((<$>), pure)
import Control.Applicative.Unicode ((⊛))
import Control.Monad.Unicode
import Data.Monoid.Unicode
import Prelude.Unicode

infixr 0 ◁
(◁) ∷ a -> Maybe a -> a
x ◁ Nothing = x
_ ◁ Just y  = y

infixr 0 ▷
(▷) ∷ Bool -> a -> Maybe a
True ▷ _ = Nothing
_    ▷ y = Just y
