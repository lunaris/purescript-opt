{-# LANGUAGE PatternSynonyms #-}

module Language.PureScript.Optimisation.JavaScript.AST.Patterns where

import qualified Language.JavaScript.Parser     as JS.P
import qualified Language.JavaScript.Parser.AST as JS.P

idAnnot :: JS.P.JSAnnot
idAnnot
  = JS.P.JSAnnot JS.P.tokenPosnEmpty [JS.P.WhiteSpace JS.P.tokenPosnEmpty " "]

pattern JSMethodCall_ meth args <- JS.P.JSMethodCall meth _ args _ _
  where
    JSMethodCall_ meth args
      = JS.P.JSMethodCall meth idAnnot args idAnnot (JS.P.JSSemi idAnnot)

pattern JSExpressionParen_ e <- JS.P.JSExpressionParen _ e _
  where
    JSExpressionParen_ e = JS.P.JSExpressionParen idAnnot e idAnnot

pattern JSFunctionExpression_ name args block <-
  JS.P.JSFunctionExpression _ name _ args _ block

  where
    JSFunctionExpression_ name args block
      = JS.P.JSFunctionExpression idAnnot name idAnnot args idAnnot block

pattern JSLCons_ x xs <- JS.P.JSLCons xs _ x
  where
    JSLCons_ x xs = JS.P.JSLCons xs idAnnot x

pattern JSBlock_ sts <- JS.P.JSBlock _ sts _
  where
    JSBlock_ sts = JS.P.JSBlock idAnnot sts idAnnot

pattern JSMemberSquare_ obj prop <- JS.P.JSMemberSquare obj _ prop _
  where
    JSMemberSquare_ obj prop
      = JS.P.JSMemberSquare obj idAnnot prop idAnnot

pattern JSMemberDot_ obj prop <- JS.P.JSMemberDot obj _ prop
  where
    JSMemberDot_ obj prop
      = JS.P.JSMemberDot obj idAnnot prop

pattern JSIdentName_ name <- JS.P.JSIdentName _ name
  where
    JSIdentName_ name = JS.P.JSIdentName idAnnot name

pattern JSAssignExpression_ lhs rhs <- JS.P.JSAssignExpression lhs _ rhs
  where
    JSAssignExpression_ lhs rhs
      = JS.P.JSAssignExpression lhs (JS.P.JSAssign idAnnot) rhs

pattern JSIdentifier_ name <- JS.P.JSIdentifier _ name
  where
    JSIdentifier_ name = JS.P.JSIdentifier idAnnot name

pattern JSStringLiteral_ s <- JS.P.JSStringLiteral _ s
  where
    JSStringLiteral_ s = JS.P.JSStringLiteral idAnnot s

pattern JSVariable_ vs <- JS.P.JSVariable _ vs _
  where
    JSVariable_ vs
      = JS.P.JSVariable idAnnot vs (JS.P.JSSemi idAnnot)

pattern JSVarInit_ e <- JS.P.JSVarInit _ e
  where
    JSVarInit_ e = JS.P.JSVarInit idAnnot e

pattern JSReturn_ me <- JS.P.JSReturn _ me _
  where
    JSReturn_ me = JS.P.JSReturn idAnnot me (JS.P.JSSemi idAnnot)

pattern JSMemberExpression_ f xs <- JS.P.JSMemberExpression f _ xs _
  where
    JSMemberExpression_ f xs
      = JS.P.JSMemberExpression f idAnnot xs idAnnot
