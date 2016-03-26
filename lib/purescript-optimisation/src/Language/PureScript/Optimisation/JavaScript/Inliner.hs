{-# LANGUAGE RankNTypes #-}

module Language.PureScript.Optimisation.JavaScript.Inliner where

import qualified Data.Generics              as G
import qualified Language.JavaScript.Parser as JS.P

data Module
  = Module
      { mName :: !String
      , mBody :: !JS.P.JSNode
      }

  deriving (Eq, Show)

jsModule :: G.GenericQ (Maybe Module)
jsModule
  = G.mkQ Nothing go
  where
    go (JS.P.NN (JS.P.JSExpression
      ( JS.P.NN (JS.P.JSExpressionParen _
          (JS.P.NN (JS.P.JSExpression (JS.P.NN
            (JS.P.JSFunctionExpression _ _ _
              [JS.P.NT (JS.P.JSIdentifier "exports") _ _] _ modBody) : _))) _)

      : JS.P.NN (JS.P.JSArguments _
          (JS.P.NN (JS.P.JSMemberSquare [JS.P.NT (JS.P.JSIdentifier "PS") _ _] _
            (JS.P.NN (JS.P.JSExpression
              [JS.P.NT (JS.P.JSStringLiteral _ modName) _ _])) _) : _) _)

      : _)))

      = Just Module
          { mName = modName
          , mBody = modBody
          }

    go _
      = Nothing

everyTopLevel :: G.GenericQ (Maybe r) -> G.GenericQ [r]
everyTopLevel f x
  = case f x of
      Nothing -> concat (G.gmapQ (everyTopLevel f) x)
      Just y  -> [y]
