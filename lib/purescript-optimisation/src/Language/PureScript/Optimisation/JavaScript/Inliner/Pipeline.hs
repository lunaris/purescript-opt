{-# LANGUAGE TupleSections #-}

module Language.PureScript.Optimisation.JavaScript.Inliner.Pipeline where

import Language.PureScript.Optimisation.JavaScript.Environment
import Language.PureScript.Optimisation.JavaScript.Inliner.Const
import Language.PureScript.Optimisation.JavaScript.Inliner.Identity
import Language.PureScript.Optimisation.JavaScript.Inliner.OperatorAliases
import Language.PureScript.Optimisation.JavaScript.Inliner.PropertyAccessors

import           Control.Arrow              ((>>>))
import qualified Language.JavaScript.Parser as JS.P

applyInlinings :: JS.P.JSAST -> JS.P.JSAST
applyInlinings ast
  = withAllDeclarations
      ( \qualifiedName
           -> (initialEnv, qualifiedName, )
          >>> apply inlineOperatorAlias
          >>> apply inlinePropertyAccessor
          >>> apply inlineId
          >>> apply inlineConstFunctor
          >>> \(_, _, e) -> e
      )

      ast

  where
    initialEnv
      = getEnvironment ast

    apply pass (env, qualifiedName, e)
      = let e' = pass env qualifiedName e
        in  (updateDecl qualifiedName e env, qualifiedName, e')
