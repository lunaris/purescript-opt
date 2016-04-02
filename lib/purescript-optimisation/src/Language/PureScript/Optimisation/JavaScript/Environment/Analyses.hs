module Language.PureScript.Optimisation.JavaScript.Environment.Analyses where

import Language.PureScript.Optimisation.JavaScript.AST

import qualified Data.Foldable                  as F
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
  = Just (decodeName x)

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

maybePlainConstructor :: JS.P.JSExpression -> Maybe [String]
maybePlainConstructor (JSFunctionExpression_ _ args (JSBlock_ sts))
  | Just argsIdents <- mapM maybeIdentIdentifier (F.toList args)
  , Just stsIdents <- mapM maybeThisAssignIdentifier sts
  , argsIdents == stsIdents

      = Just argsIdents

maybePlainConstructor _
  = Nothing

maybeThisAssignIdentifier :: JS.P.JSStatement -> Maybe String
maybeThisAssignIdentifier (JSAssignStatement_
  (JSMemberDot_ JSThisLiteral (JSIdentifier_ name))
  (JSIdentifier_ name'))

  | name == name'
      = Just name

maybeThisAssignIdentifier (JSAssignStatement_
  (JSMemberSquare_ JSThisLiteral (JSStringLiteral_ propName))
  (JSIdentifier_ argName))

  | propName' <- stringLiteralString propName
  , argName' <- decodeName argName
  , propName' == argName'

      = Just propName'

maybeThisAssignIdentifier _
  = Nothing
