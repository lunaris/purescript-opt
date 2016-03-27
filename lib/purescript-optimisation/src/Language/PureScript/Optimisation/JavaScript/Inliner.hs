{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}

module Language.PureScript.Optimisation.JavaScript.Inliner where

import qualified Data.Generics              as G
import qualified Data.Map.Strict            as M
import qualified Data.Text                  as T
import qualified Language.JavaScript.Parser as JS.P

maybeOperatorAlias :: JS.P.JSNode -> Maybe String
maybeOperatorAlias (JSFunctionExpression_ _ args
  (JSBlock_ [JSReturn_ (JSApplication_ f args')]))

  | Just argsIdents <- mapM maybeIdentifier args
  , Just args'Idents <- mapM maybeIdentifier args
  , Just fIdent <- maybeIdentifier f

      = Just fIdent

maybeOperatorAlias _
  = Nothing

maybeIdentifier :: JS.P.JSNode -> Maybe String
maybeIdentifier (JSIdentifier_ x)
  = Just x

maybeIdentifier _
  = Nothing

maybePropertyAccessor :: JS.P.JSNode -> Maybe String
maybePropertyAccessor (JSFunctionExpression_ _ [JSIdentifier_ argName]
  (JSBlock_ [JSReturn_ (JSExpression_
    [JSMemberDot_ (JSIdentifier_ argName') (JSIdentifier_ prop)])]))

  | argName == argName'
      = Just prop

maybePropertyAccessor _
  = Nothing

data QualifiedName
  = QualifiedName
      { qnModule  :: !T.Text
      , qnName    :: !T.Text
      }

  deriving (Eq, Ord, Show)

pattern JSExpression_ :: [JS.P.JSNode] -> JS.P.JSNode
pattern JSExpression_ es <- JS.P.NN (JS.P.JSExpression es)

pattern JSReturn_ :: JS.P.JSNode -> JS.P.JSNode
pattern JSReturn_ v <-
  JS.P.NN (JS.P.JSReturn _ [v] _)

pattern JSBlock_ :: [JS.P.JSNode] -> JS.P.JSNode
pattern JSBlock_ sts <- JS.P.NN (JS.P.JSBlock _ sts _)

pattern JSFunctionExpression_ :: [JS.P.JSNode]
                              -> [JS.P.JSNode]
                              -> JS.P.JSNode
                              -> JS.P.JSNode

pattern JSFunctionExpression_ name args body <-
  JS.P.NN (JS.P.JSFunctionExpression _ name _ args _ body)

pattern JSIdentifier_ :: String -> JS.P.JSNode
pattern JSIdentifier_ s <- JS.P.NT (JS.P.JSIdentifier s) _ _

pattern JSVarDeclInit_ :: String -> JS.P.JSNode -> JS.P.JSNode
pattern JSVarDeclInit_ name def <-
  JS.P.NN (JS.P.JSVarDecl (JSIdentifier_ name) [_, def])

pattern JSMemberDot_ :: JS.P.JSNode -> JS.P.JSNode -> JS.P.JSNode
pattern JSMemberDot_ obj prop <- JS.P.NN (JS.P.JSMemberDot [obj] _ prop)

pattern JSMemberSquare_ :: JS.P.JSNode -> JS.P.JSNode -> JS.P.JSNode
pattern JSMemberSquare_ obj prop <-
  JS.P.NN (JS.P.JSMemberSquare [obj] _ prop _)

pattern JSStringLiteral_ :: String -> JS.P.JSNode
pattern JSStringLiteral_ s <- JS.P.NT (JS.P.JSStringLiteral _ s) _ _

getAllDecls :: JS.P.JSNode -> M.Map QualifiedName JS.P.JSNode
getAllDecls
  = M.fromList . everyTopLevel getAllDeclsQ

pattern JSApplication_ :: JS.P.JSNode -> [JS.P.JSNode] -> JS.P.JSNode
pattern JSApplication_ fe fxs <-
  JS.P.NN (JS.P.JSExpression (fe : JS.P.NN (JS.P.JSArguments _ fxs _) : _))

getAllDeclsQ :: G.GenericQ (Maybe [(QualifiedName, JS.P.JSNode)])
getAllDeclsQ
  = G.mkQ Nothing go
  where
    go (JSApplication_
      (JS.P.NN (JS.P.JSExpressionParen _
        (JS.P.NN (JS.P.JSExpression
          (JSFunctionExpression_ _ [JSIdentifier_ "exports"] modBody : _))) _))

      (JSMemberSquare_ (JSIdentifier_ "PS")
        (JS.P.NN (JS.P.JSExpression [JSStringLiteral_ modName])) : _))

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
        go (JSVarDeclInit_ declName declDef)
          = case declDef of
              JSMemberSquare_ _ _ ->
                Nothing

              _ ->
                Just
                  [ ( QualifiedName modName (T.pack (decodeName declName))
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

decodeName :: String -> String
decodeName []
  = []

decodeName s@('$' : '$' : jsReserved)
  = jsReserved

decodeName s@('$' : cs)
  = decodeSymbol nextSym ++ decodeName rest
  where
    (nextSym, rest) = break (== '$') cs

decodeName s
  = s

decodeSymbol :: String -> String
decodeSymbol "_"       = "_"
decodeSymbol "dot"     = "."
decodeSymbol "dollar"  = "$"
decodeSymbol "tilde"   = "~"
decodeSymbol "eq"      = "="
decodeSymbol "less"    = "<"
decodeSymbol "greater" = ">"
decodeSymbol "bang"    = "!"
decodeSymbol "hash"    = "#"
decodeSymbol "percent" = "%"
decodeSymbol "up"      = "^"
decodeSymbol "amp"     = "&"
decodeSymbol "bar"     = "|"
decodeSymbol "times"   = "*"
decodeSymbol "div"     = "/"
decodeSymbol "plus"    = "+"
decodeSymbol "minus"   = "-"
decodeSymbol "colon"   = ":"
decodeSymbol "bslash"  = "\\"
decodeSymbol "qmark"   = "?"
decodeSymbol "at"      = "@"
decodeSymbol "prime"   = "\'"
decodeSymbol s         = s
