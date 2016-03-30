{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}

module Language.PureScript.Optimisation.JavaScript.Inliner where

import qualified Data.Generics                  as G
import qualified Data.Maybe                     as Mb
import qualified Data.Map.Strict                as M
import qualified Data.Text                      as T
import qualified Language.JavaScript.Parser     as JS.P
import qualified Language.JavaScript.Parser.AST as JS.P

pattern JSMethodCall_ meth args <- JS.P.JSMethodCall meth _ args _ _
  where
    JSMethodCall_ meth args
      = JS.P.JSMethodCall meth JS.P.JSNoAnnot args JS.P.JSNoAnnot
          (JS.P.JSSemi JS.P.JSNoAnnot)

pattern JSExpressionParen_ e <- JS.P.JSExpressionParen _ e _
  where
    JSExpressionParen_ e
      = JS.P.JSExpressionParen JS.P.JSNoAnnot e JS.P.JSNoAnnot

pattern JSFunctionExpression_ name args block <-
  JS.P.JSFunctionExpression _ name _ args _ block

  where
    JSFunctionExpression_ name args block
      = JS.P.JSFunctionExpression JS.P.JSNoAnnot name JS.P.JSNoAnnot
          args JS.P.JSNoAnnot block

pattern JSLCons_ x xs <- JS.P.JSLCons xs _ x
  where
    JSLCons_ x xs = JS.P.JSLCons xs JS.P.JSNoAnnot x

pattern JSBlock_ sts <- JS.P.JSBlock _ sts _
  where
    JSBlock_ sts = JS.P.JSBlock JS.P.JSNoAnnot sts JS.P.JSNoAnnot

pattern JSMemberSquare_ obj prop <- JS.P.JSMemberSquare obj _ prop _
  where
    JSMemberSquare_ obj prop
      = JS.P.JSMemberSquare obj JS.P.JSNoAnnot prop JS.P.JSNoAnnot

pattern JSMemberDot_ obj prop <- JS.P.JSMemberDot obj _ prop
  where
    JSMemberDot_ obj prop
      = JS.P.JSMemberDot obj JS.P.JSNoAnnot prop

pattern JSIdentName_ name <- JS.P.JSIdentName _ name
  where
    JSIdentName_ name = JS.P.JSIdentName JS.P.JSNoAnnot name

pattern JSAssignExpression_ lhs rhs <- JS.P.JSAssignExpression lhs _ rhs
  where
    JSAssignExpression_ lhs rhs
      = JS.P.JSAssignExpression lhs (JS.P.JSAssign JS.P.JSNoAnnot) rhs

pattern JSIdentifier_ name <- JS.P.JSIdentifier _ name
  where
    JSIdentifier_ name = JS.P.JSIdentifier JS.P.JSNoAnnot name

pattern JSStringLiteral_ s <- JS.P.JSStringLiteral _ s
  where
    JSStringLiteral_ s = JS.P.JSStringLiteral JS.P.JSNoAnnot s

pattern JSVariable_ vs <- JS.P.JSVariable _ vs _
  where
    JSVariable_ vs
      = JS.P.JSVariable JS.P.JSNoAnnot vs (JS.P.JSSemi JS.P.JSNoAnnot)

pattern JSVarInit_ e <- JS.P.JSVarInit _ e
  where
    JSVarInit_ e = JS.P.JSVarInit JS.P.JSNoAnnot e

pattern JSReturn_ me <- JS.P.JSReturn _ me _
  where
    JSReturn_ me = JS.P.JSReturn JS.P.JSNoAnnot me (JS.P.JSSemi JS.P.JSNoAnnot)

pattern JSMemberExpression_ f xs <- JS.P.JSMemberExpression f _ xs _
  where
    JSMemberExpression_ f xs
      = JS.P.JSMemberExpression f JS.P.JSNoAnnot xs JS.P.JSNoAnnot

getAllDeclMetas :: JS.P.JSAST -> M.Map QualifiedName DeclMeta
getAllDeclMetas
  = M.fromList . everyTopLevel getAllDeclMetasQ

getAllDeclMetasQ :: G.GenericQ (Maybe [(QualifiedName, DeclMeta)])
getAllDeclMetasQ
  = G.mkQ Nothing go
  where
    go (JSMethodCall_
      (JSExpressionParen_ (JSFunctionExpression_ _
        (JS.P.JSLOne (JSIdentName_ "exports")) modBody))

      (JS.P.JSLOne (JSAssignExpression_
        (JSMemberSquare_ (JSIdentifier_ "PS") (JSStringLiteral_ modName)) _)))

      = Just (getModuleDeclMetas (T.pack (tail (init modName))) modBody)

    go _
      = Nothing

getModuleDeclMetas :: T.Text -> JS.P.JSBlock -> [(QualifiedName, DeclMeta)]
getModuleDeclMetas modName
  = everyTopLevel getModuleDeclMetasQ
  where
    getModuleDeclMetasQ :: G.GenericQ (Maybe [(QualifiedName, DeclMeta)])
    getModuleDeclMetasQ
      = G.mkQ Nothing go
      where
        go (JSVariable_ (JS.P.JSLOne
          (JS.P.JSVarInitExpression
            (JSIdentifier_ declName) (JSVarInit_ declDef))))

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

data QualifiedName
  = QualifiedName
      { qnModule  :: !T.Text
      , qnName    :: !T.Text
      }

  deriving (Eq, Ord, Show)

data DeclMeta
  = DeclMeta
      { dmName              :: !QualifiedName
      , dmDefinition        :: !JS.P.JSExpression
      , dmOperatorAlias     :: !(Maybe String)
      , dmPropertyAccessor  :: !(Maybe String)
      }

  deriving (Eq, Show)

maybeOperatorAlias :: JS.P.JSExpression -> Maybe String
maybeOperatorAlias (JSFunctionExpression_ _ args
  (JSBlock_ [JSReturn_ (Just (JSMemberExpression_ f args'))]))

  | Just argsIdents <- mapM maybeIdentIdentifier args
  , Just args'Idents <- mapM maybeExpressionIdentifier args'
  , argsIdents == args'Idents

  , Just fIdent <- maybeExpressionIdentifier f

      = Just fIdent

maybeOperatorAlias _
  = Nothing

maybeIdentIdentifier :: JS.P.JSIdent -> Maybe String
maybeIdentIdentifier (JSIdentName_ x)
  = Just x

maybeIdentIdentifier _
  = Nothing

deriving instance Functor JS.P.JSCommaList
deriving instance Foldable JS.P.JSCommaList
deriving instance Traversable JS.P.JSCommaList

maybeExpressionIdentifier :: JS.P.JSExpression -> Maybe String
maybeExpressionIdentifier (JSIdentifier_ x)
  = Just x

maybeExpressionIdentifier _
  = Nothing

maybePropertyAccessor :: JS.P.JSExpression -> Maybe String
maybePropertyAccessor (JSFunctionExpression_ _
  (JS.P.JSLOne (JSIdentName_ argName))
  (JSBlock_ [JSReturn_ (Just
    (JSMemberDot_ (JSIdentifier_ argName') (JSIdentifier_ prop)))]))

  | argName == argName'
      = Just prop

maybePropertyAccessor _
  = Nothing

test :: FilePath -> FilePath -> IO ()
test inFile outFile
  = do
      ast <- JS.P.parseFile inFile
      let metas = getAllDeclMetas ast
          ast'  = withAllDeclMetas (inlineOperatorAlias metas) ast

      writeFile outFile (JS.P.renderToString ast')

inlineOperatorAlias :: M.Map QualifiedName DeclMeta
                    -> QualifiedName
                    -> JS.P.JSExpression
                    -> JS.P.JSExpression

inlineOperatorAlias metas name x
  = Mb.fromMaybe x $ do
      DeclMeta{..} <- M.lookup name metas
      opAlias <- dmOperatorAlias
      pure (JSIdentifier_ opAlias)

withAllDeclMetas  :: (QualifiedName -> JS.P.JSExpression -> JS.P.JSExpression)
                  -> JS.P.JSAST
                  -> JS.P.JSAST

withAllDeclMetas f
  = atEveryTopLevel withAllDeclMetasM
  where
    withAllDeclMetasM :: G.GenericM Maybe
    withAllDeclMetasM
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
                (withModuleDeclMetas (T.pack (tail (init modName))) modBody)))

              ae)

        go _
          = Nothing

    withModuleDeclMetas :: T.Text -> JS.P.JSBlock -> JS.P.JSBlock
    withModuleDeclMetas modName
      = atEveryTopLevel withModuleDeclMetasM
      where
        withModuleDeclMetasM :: G.GenericM Maybe
        withModuleDeclMetasM
          = G.mkMp go
          where
            go (JSVariable_ (JS.P.JSLOne
              (JS.P.JSVarInitExpression
                (JSIdentifier_ declName) (JSVarInit_ declDef))))

              = case declDef of
                  JSMemberSquare_ _ _ ->
                    Nothing

                  _ ->
                    let name
                          = QualifiedName modName (T.pack (decodeName declName))

                    in  Just (JSVariable_ (JS.P.JSLOne
                          (JS.P.JSVarInitExpression (JSIdentifier_ declName)
                            (JSVarInit_ (f name declDef)))))

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
