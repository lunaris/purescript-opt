module Language.PureScript.Optimisation.JavaScript.Environment.Analyses where

import Language.PureScript.Optimisation.JavaScript.AST

import qualified Language.JavaScript.Parser.AST as JS.P

maybeOperatorAlias :: JS.P.JSExpression -> Maybe String
maybeOperatorAlias (JSFunctionExpression_ _ args
  (JSBlock_ [JSReturn_ (Just (JSMemberExpression_ f args'))]))

  | Just argsIdents <- mapM maybeIdentIdentifier args
  , Just args'Idents <- mapM maybeExpressionIdentifier args'
  , argsIdents == args'Idents

  , Just fIdent <- maybeExpressionIdentifier f

      = Just fIdent

maybeOperatorAlias _
  = Nothing

maybeIdentIdentifier :: JS.P.JSIdent -> Maybe String
maybeIdentIdentifier (JSIdentName_ x)
  = Just x

maybeIdentIdentifier _
  = Nothing

maybeExpressionIdentifier :: JS.P.JSExpression -> Maybe String
maybeExpressionIdentifier (JSIdentifier_ x)
  = Just x

maybeExpressionIdentifier _
  = Nothing

maybePropertyAccessor :: JS.P.JSExpression -> Maybe String
maybePropertyAccessor (JSFunctionExpression_ _
  (JS.P.JSLOne (JSIdentName_ argName))
  (JSBlock_ [JSReturn_ (Just
    (JSMemberDot_ (JSIdentifier_ argName') (JSIdentifier_ prop)))]))

  | argName == argName'
      = Just prop

maybePropertyAccessor _
  = Nothing
