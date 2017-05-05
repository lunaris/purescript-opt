module Language.PureScript.Optimisation.JavaScript.Inliner.PropertyAccessors where

import Language.PureScript.Optimisation.JavaScript.AST
import Language.PureScript.Optimisation.JavaScript.Environment

import qualified Data.Generics                  as G
import qualified Data.Maybe                     as Mb
import qualified Data.Text                      as T
import qualified Language.JavaScript.Parser     as JS.P
import qualified Language.JavaScript.Parser.AST as JS.P

-- |Inline applications of functions which only serve as property accessors,
--  e.g.:
--
--  @
--  var getProp = function getProp(x) {
--    return x.prop;
--  };
--  @
inlinePropertyAccessor  :: Environment
                        -> QualifiedName
                        -> JS.P.JSExpression
                        -> JS.P.JSExpression

inlinePropertyAccessor env (QualifiedName modName _)
  = G.everywhere inlinePropertyAccessor'
  where
    inlinePropertyAccessor' :: G.GenericT
    inlinePropertyAccessor'
      = G.mkT go
      where
        go :: JS.P.JSExpression -> JS.P.JSExpression
        go e@(JSMemberExpression_
          (JSMemberSquare_ (JSIdentifier_ objName) (JSStringLiteral_ propName))
          (JS.P.JSLOne arg))

          = Mb.fromMaybe e $ do
              let qualifiedImportName = QualifiedName modName (T.pack objName)
              importModName <- lookupImportIdentifier qualifiedImportName env

              let qualifiedFunName
                    = QualifiedName importModName
                        (T.pack (stringLiteralString propName))

              propAccessor <- lookupPropertyAccessor qualifiedFunName env
              pure (JSMemberDot_ arg (JSIdentifier_ propAccessor))

        go e
          = e
