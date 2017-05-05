module Language.PureScript.Optimisation.JavaScript.Inliner.OperatorAliases where

import Language.PureScript.Optimisation.JavaScript.Environment

import qualified Data.Maybe                 as Mb
import qualified Data.Text                  as T
import qualified Language.JavaScript.Parser as JS.P

-- |Inline functions which are simply aliases of other functions. This is
--  common with operator aliases, such as that of @(<$>)@ to @map@:
--
--  @
--  var $less$dollar$greater = function (dictFunctor) {
--    return map(dictFunctor);
--  };
--  @
inlineOperatorAlias :: Environment
                    -> QualifiedName
                    -> JS.P.JSExpression
                    -> JS.P.JSExpression

inlineOperatorAlias env qualifiedName@(QualifiedName modName _) e
  = Mb.fromMaybe e $ do
      opAlias <- lookupOperatorAlias qualifiedName env
      let qualifiedAliasName = QualifiedName modName (T.pack opAlias)
      lookupDecl qualifiedAliasName env
