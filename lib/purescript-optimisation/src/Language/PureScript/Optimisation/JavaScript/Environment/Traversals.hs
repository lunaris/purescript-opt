module Language.PureScript.Optimisation.JavaScript.Environment.Traversals where

import Language.PureScript.Optimisation.JavaScript.AST
import Language.PureScript.Optimisation.JavaScript.Environment.Types
import Language.PureScript.Optimisation.JavaScript.Schemes

import qualified Data.Generics                  as G
import qualified Data.Text                      as T
import qualified Language.JavaScript.Parser.AST as JS.P

withAllDeclarations :: (QualifiedName -> JS.P.JSExpression -> JS.P.JSExpression)
                    -> JS.P.JSAST
                    -> JS.P.JSAST

withAllDeclarations f
  = atEveryTopLevel withAllDeclarationsM
  where
    withAllDeclarationsM :: G.GenericM Maybe
    withAllDeclarationsM
      = G.mkMp go
      where
        go :: JS.P.JSStatement -> Maybe JS.P.JSStatement
        go (JSMethodCall_
          (JSExpressionParen_ (JSFunctionExpression_ name
            (JS.P.JSLOne (JSIdentName_ "exports")) modBody))

          ae@(JS.P.JSLOne (JSAssignExpression_
            (JSMemberSquare_ (JSIdentifier_ "PS") (JSStringLiteral_ modName))
              _)))

          = Just (JSMethodCall_
              (JSExpressionParen_ (JSFunctionExpression_ name
                (JS.P.JSLOne (JSIdentName_ "exports"))
                (withModuleDeclarations (T.pack (stringLiteralString modName))
                  modBody)))

              ae)

        go _
          = Nothing

    withModuleDeclarations :: T.Text -> JS.P.JSBlock -> JS.P.JSBlock
    withModuleDeclarations modName
      = atEveryTopLevel withModuleDeclarationsM
      where
        withModuleDeclarationsM :: G.GenericM Maybe
        withModuleDeclarationsM
          = G.mkMp go
          where
            go (JSVariable_ (JS.P.JSLOne
              (JS.P.JSVarInitExpression
                (JSIdentifier_ declName) (JSVarInit_ declDef))))

              = case declDef of
                  JSMemberSquare_ _ _ ->
                    Nothing

                  _ ->
                    let qualifiedName
                          = QualifiedName modName (T.pack (decodeName declName))

                    in  Just (JSVariable_ (JS.P.JSLOne
                          (JS.P.JSVarInitExpression (JSIdentifier_ declName)
                            (JSVarInit_ (f qualifiedName declDef)))))

            go _
              = Nothing
