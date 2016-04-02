module Language.PureScript.Optimisation.JavaScript.Inliner.Pipeline where

import Language.PureScript.Optimisation.JavaScript.Environment
import Language.PureScript.Optimisation.JavaScript.Inliner.Identity
import Language.PureScript.Optimisation.JavaScript.Inliner.OperatorAliases
import Language.PureScript.Optimisation.JavaScript.Inliner.PropertyAccessors

import qualified Language.JavaScript.Parser as JS.P

applyInlinings :: JS.P.JSAST -> JS.P.JSAST
applyInlinings ast
  = withAllDeclarations
      ( \qn
          -> inlineId env qn
          .  inlinePropertyAccessor env qn
          .  inlineOperatorAlias env qn
      )

      ast

  where
    env = getEnvironment ast
