{-# LANGUAGE OverloadedStrings #-}

module Language.PureScript.Optimisation.JavaScript.Inliner.Const where

import Language.PureScript.Optimisation.JavaScript.AST
import Language.PureScript.Optimisation.JavaScript.Environment

import           Control.Monad                  (guard)
import qualified Data.Generics                  as G
import qualified Data.Maybe                     as Mb
import qualified Data.Text                      as T
import qualified Language.JavaScript.Parser     as JS.P
import qualified Language.JavaScript.Parser.AST as JS.P
import Debug.Trace

-- |Inline applications of terms to the @Functor@ dictionary for @Data.Const@ in
--  cases where there is a sizeable reduction in code. This is often possible
--  due to the fact that large calls to @map@ may be elided. For example, in:
--
--  @
--  return dictFunctor.map(function (x) {
--    ...
--  })(o);
--  @
--
--  If @dictFunctor@ is the dictionary for @Data.Const@, @map(f)(o)@ is
--  equivalent to @o@ and thus the AST may be collapsed substantially.
inlineConstFunctor  :: Environment
                    -> QualifiedName
                    -> JS.P.JSExpression
                    -> JS.P.JSExpression

inlineConstFunctor env (QualifiedName modName _)
  = G.everywhere inlineConstFunctor'
  where
    inlineConstFunctor' :: G.GenericT
    inlineConstFunctor'
      = G.mkT go
      where
        go :: JS.P.JSExpression -> JS.P.JSExpression
        go e@(JSMemberExpression_
          (JSMemberDot_ (JSIdentifier_ funModName) (JSIdentifier_ funName))
          (JS.P.JSLOne (JSMemberDot_
            (JSIdentifier_ dictModName) (JSIdentifier_ dictName))))

          = Mb.fromMaybe e $ do
              dictModName' <- resolveImportIdentifier modName dictModName env
              guard (dictModName' == "Data.Const" && dictName == "functorConst")

              funModName' <- resolveImportIdentifier modName funModName env
              let qualifiedFunName
                    = QualifiedName funModName' (T.pack funName)

              funDecl <- lookupDecl qualifiedFunName env
              pure (specialiseFunDecl $ (if funName == "cpfpMonthlyRepayment" then traceShow funDecl else id) funDecl)

        go e
          = e

    specialiseFunDecl (JSFunctionExpression_ _
      (JS.P.JSLOne (JSIdentName_ dict))
      (JSBlock_ [JSReturn_ (Just e)]))

      = G.everywhere specialiseFunDecl' e
      where
        specialiseFunDecl' :: G.GenericT
        specialiseFunDecl'
          = G.mkT go
          where
            go (JSCallExpression_
              (JSMemberExpression_
                (JSMemberDot_ (JSIdentifier_ dict') (JSIdentifier_ mapFun)) _)

              (JS.P.JSLOne mappedArg))

              | dict == dict' && mapFun == "map"
                  = mappedArg

            go x
              = x

    specialiseFunDecl e
      = e
