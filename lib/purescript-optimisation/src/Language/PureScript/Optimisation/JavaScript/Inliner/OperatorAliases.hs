module Language.PureScript.Optimisation.JavaScript.Inliner.OperatorAliases where

import Language.PureScript.Optimisation.JavaScript.Environment

import qualified Data.Maybe                 as Mb
import qualified Data.Text                  as T
import qualified Language.JavaScript.Parser as JS.P

inlineOperatorAlias :: Environment
                    -> QualifiedName
                    -> JS.P.JSExpression
                    -> JS.P.JSExpression

inlineOperatorAlias env qualifiedName@(QualifiedName modName _) e
  = Mb.fromMaybe e $ do
      opAlias <- lookupOperatorAlias qualifiedName env
      let qualifiedAliasName = QualifiedName modName (T.pack opAlias)
      lookupDecl qualifiedAliasName env
