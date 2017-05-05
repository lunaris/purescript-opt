module Language.PureScript.Optimisation.JavaScript.Inliner.Identity where

import Language.PureScript.Optimisation.JavaScript.AST
import Language.PureScript.Optimisation.JavaScript.Environment

import           Control.Monad                  (guard)
import qualified Data.Generics                  as G
import qualified Data.Maybe                     as Mb
import qualified Data.Text                      as T
import qualified Language.JavaScript.Parser     as JS.P
import qualified Language.JavaScript.Parser.AST as JS.P

-- |Inline applications of functions which have been identified as
--  operationally identical to the identity function; that is to say, functions
--  defined in a manner analogous to:
--
--  @
--  var someId = function someId(x) {
--    return x;
--  };
--  @
inlineId  :: Environment
          -> QualifiedName
          -> JS.P.JSExpression
          -> JS.P.JSExpression

inlineId env (QualifiedName modName _)
  = G.everywhere inlineId'
  where
    inlineId' :: G.GenericT
    inlineId'
      = G.mkT go
      where
        go :: JS.P.JSExpression -> JS.P.JSExpression
        go e@(JSMemberExpression_
          (JSMemberDot_ (JSIdentifier_ objName) (JSIdentifier_ propName))
          (JS.P.JSLOne arg))

          = Mb.fromMaybe e $ do
              let qualifiedImportName = QualifiedName modName (T.pack objName)
              importModName <- lookupImportIdentifier qualifiedImportName env

              let qualifiedFunName
                    = QualifiedName importModName (T.pack propName)

              guard (lookupIsIdentity qualifiedFunName env)
              pure arg

        go e
          = e
