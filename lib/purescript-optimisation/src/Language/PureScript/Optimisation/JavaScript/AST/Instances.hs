{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.PureScript.Optimisation.JavaScript.AST.Instances where

import qualified Language.JavaScript.Parser.AST as JS.P

deriving instance Functor JS.P.JSCommaList
deriving instance Foldable JS.P.JSCommaList
deriving instance Traversable JS.P.JSCommaList
