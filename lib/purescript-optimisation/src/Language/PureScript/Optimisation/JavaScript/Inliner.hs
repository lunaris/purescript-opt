{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}

module Language.PureScript.Optimisation.JavaScript.Inliner where

import qualified Data.Generics              as G
import qualified Data.Map.Strict            as M
import qualified Data.Text                  as T
import qualified Language.JavaScript.Parser as JS.P

data Environment
  = Environment
      { eDecls :: !(M.Map QualifiedName JS.P.JSNode)
      }

  deriving (Eq, Show)

data QualifiedName
  = QualifiedName
      { qnModule  :: !T.Text
      , qnName    :: !T.Text
      }

  deriving (Eq, Ord, Show)

getAllDecls :: JS.P.JSNode -> M.Map QualifiedName JS.P.JSNode
getAllDecls
  = M.fromList . everyTopLevel getAllDeclsQ

getAllDeclsQ :: G.GenericQ (Maybe [(QualifiedName, JS.P.JSNode)])
getAllDeclsQ
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

      : _
      )))

      = Just (getModuleDecls (T.pack modName) modBody)

    go _
      = Nothing

getModuleDecls :: T.Text -> JS.P.JSNode -> [(QualifiedName, JS.P.JSNode)]
getModuleDecls modName
  = everyTopLevel getModuleDeclsQ
  where
    getModuleDeclsQ :: G.GenericQ (Maybe [(QualifiedName, JS.P.JSNode)])
    getModuleDeclsQ
      = G.mkQ Nothing go
      where
        go (JS.P.NN (JS.P.JSVarDecl (JS.P.NT (JS.P.JSIdentifier declName) _ _)
          [_, declDef]))

          = case declDef of
              JS.P.NN (JS.P.JSMemberSquare _ _ _ _) ->
                Nothing

              _ ->
                Just
                  [ ( QualifiedName modName (T.pack declName)
                    , declDef
                    )

                  ]

        go _
          = Nothing

everyTopLevel :: G.GenericQ (Maybe [r]) -> G.GenericQ [r]
everyTopLevel f x
  = case f x of
      Nothing -> concat (G.gmapQ (everyTopLevel f) x)
      Just ys -> ys
