{-# LANGUAGE RankNTypes #-}

module Language.PureScript.Optimisation.JavaScript.Schemes where

import qualified Data.Generics as G

atEveryTopLevel :: G.GenericM Maybe -> G.GenericT
atEveryTopLevel f x
  = case f x of
      Nothing -> G.gmapT (atEveryTopLevel f) x
      Just y  -> y

everyTopLevel :: Monoid m => G.GenericQ (Maybe m) -> G.GenericQ m
everyTopLevel f x
  = case f x of
      Nothing -> G.gmapQl mappend mempty (everyTopLevel f) x
      Just ys -> ys
