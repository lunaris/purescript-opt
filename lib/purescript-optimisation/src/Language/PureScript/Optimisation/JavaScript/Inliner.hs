{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}

module Language.PureScript.Optimisation.JavaScript.Inliner where

import qualified Data.Generics              as G
import qualified Data.Maybe                 as Mb
import qualified Data.Map.Strict            as M
import qualified Data.Text                  as T
import qualified Language.JavaScript.Parser as JS.P

test :: FilePath -> FilePath -> IO ()
test inFile outFile
  = do
      ast <- JS.P.parseFile inFile
      let metas = getAllDeclMetas ast
          ast' = withAllDeclMetas (inlineOperatorAlias metas) ast

      writeFile outFile (JS.P.renderToString ast')

inlineOperatorAlias :: M.Map QualifiedName DeclMeta
                    -> QualifiedName
                    -> JS.P.JSNode
                    -> JS.P.JSNode

inlineOperatorAlias metas name x
  = Mb.fromMaybe x $ do
      DeclMeta{..} <- M.lookup name metas
      opAlias <- dmOperatorAlias
      pure (JS.P.NT (JS.P.JSIdentifier opAlias) (JS.P.TokenPn 29 7 30) [])

maybeOperatorAlias :: JS.P.JSNode -> Maybe String
maybeOperatorAlias (JSFunctionExpression_ _ args
  (JSBlock_ [JSReturn_ (JSApplication_ f args')]))

  | Just argsIdents <- mapM maybeIdentifier args
  , Just args'Idents <- mapM maybeIdentifier args'
  , argsIdents == args'Idents

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

data DeclMeta
  = DeclMeta
      { dmName              :: !QualifiedName
      , dmDefinition        :: !JS.P.JSNode
      , dmOperatorAlias     :: !(Maybe String)
      , dmPropertyAccessor  :: !(Maybe String)
      }

  deriving (Eq, Show)

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

pattern JSApplication_ :: JS.P.JSNode -> [JS.P.JSNode] -> JS.P.JSNode
pattern JSApplication_ fe fxs <-
  JS.P.NN (JS.P.JSExpression (fe : JS.P.NN (JS.P.JSArguments _ fxs _) : _))

getAllDeclMetas :: JS.P.JSNode -> M.Map QualifiedName DeclMeta
getAllDeclMetas
  = M.fromList . everyTopLevel getAllDeclMetasQ

getAllDeclMetasQ :: G.GenericQ (Maybe [(QualifiedName, DeclMeta)])
getAllDeclMetasQ
  = G.mkQ Nothing go
  where
    go (JSApplication_
      (JS.P.NN (JS.P.JSExpressionParen _
        (JS.P.NN (JS.P.JSExpression
          (JSFunctionExpression_ _ [JSIdentifier_ "exports"] modBody : _))) _))

      (JSMemberSquare_ (JSIdentifier_ "PS")
        (JS.P.NN (JS.P.JSExpression [JSStringLiteral_ modName])) : _))

      = Just (getModuleDeclMetas (T.pack modName) modBody)

    go _
      = Nothing

getModuleDeclMetas :: T.Text -> JS.P.JSNode -> [(QualifiedName, DeclMeta)]
getModuleDeclMetas modName
  = everyTopLevel getModuleDeclMetasQ
  where
    getModuleDeclMetasQ :: G.GenericQ (Maybe [(QualifiedName, DeclMeta)])
    getModuleDeclMetasQ
      = G.mkQ Nothing go
      where
        go (JSVarDeclInit_ declName declDef)
          = case declDef of
              JSMemberSquare_ _ _ ->
                Nothing

              _ ->
                let name = QualifiedName modName (T.pack (decodeName declName))
                in  Just
                      [ ( name
                        , DeclMeta
                            { dmName              = name
                            , dmDefinition        = declDef
                            , dmOperatorAlias     = maybeOperatorAlias declDef
                            , dmPropertyAccessor  =
                                maybePropertyAccessor declDef

                            }

                        )

                      ]

        go _
          = Nothing

withAllDeclMetas  :: (QualifiedName -> JS.P.JSNode -> JS.P.JSNode)
                  -> JS.P.JSNode
                  -> JS.P.JSNode

withAllDeclMetas f
  = atEveryTopLevel withAllDeclMetasM
  where
    withAllDeclMetasM :: G.GenericM Maybe
    withAllDeclMetasM
      = G.mkMp go
      where
        go (JSApplication_
          (JS.P.NN (JS.P.JSExpressionParen _
            (JS.P.NN (JS.P.JSExpression
              (JSFunctionExpression_ _ [JSIdentifier_ "exports"] modBody : _))) _))

          (JSMemberSquare_ (JSIdentifier_ "PS")
            (JS.P.NN (JS.P.JSExpression [JSStringLiteral_ modName])) : _))

          = Just (withModuleDeclMetas (T.pack modName) modBody)

        go _
          = Nothing

    withModuleDeclMetas :: T.Text -> JS.P.JSNode -> JS.P.JSNode
    withModuleDeclMetas modName
      = atEveryTopLevel withModuleDeclMetasM
      where
        withModuleDeclMetasM :: G.GenericM Maybe
        withModuleDeclMetasM
          = G.mkMp go
          where
            go (JS.P.NN (JS.P.JSVarDecl
              declNameJSNode@(JS.P.NT (JS.P.JSIdentifier declName) _ _) [eqSym, declDef]))

              = case declDef of
                  JSMemberSquare_ _ _ ->
                    Nothing

                  _ ->
                    let name
                          = QualifiedName modName (T.pack (decodeName declName))

                    in  Just (JS.P.NN (JS.P.JSVarDecl declNameJSNode [eqSym,
                          f name declDef]))

            go _
              = Nothing

atEveryTopLevel :: G.GenericM Maybe -> G.GenericT
atEveryTopLevel f x
  = case f x of
      Nothing -> G.gmapT (atEveryTopLevel f) x
      Just y  -> y

everyTopLevel :: G.GenericQ (Maybe [r]) -> G.GenericQ [r]
everyTopLevel f x
  = case f x of
      Nothing -> concat (G.gmapQ (everyTopLevel f) x)
      Just ys -> ys

decodeName :: String -> String
decodeName []
  = []

decodeName ('$' : '$' : jsReserved)
  = jsReserved

decodeName ('$' : cs)
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
