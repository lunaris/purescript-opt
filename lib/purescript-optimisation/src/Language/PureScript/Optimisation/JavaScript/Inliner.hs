{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.PureScript.Optimisation.JavaScript.Inliner where

import qualified Data.Generics                  as G
import qualified Data.Maybe                     as Mb
import qualified Data.Map.Strict                as M
import qualified Data.Monoid                    as Mon
import qualified Data.Text                      as T
import qualified Language.JavaScript.Parser     as JS.P
import qualified Language.JavaScript.Parser.AST as JS.P

data Environment
  = Environment
      { eDeclarations :: !(M.Map QualifiedName Declaration)
      , eImports      :: !(M.Map QualifiedName ModuleName)
      }

emptyEnvironment :: Environment
emptyEnvironment
  = Environment
      { eDeclarations = M.empty
      , eImports      = M.empty
      }

lookupDecl :: QualifiedName -> Environment -> Maybe Declaration
lookupDecl qualifiedName Environment{..}
  = M.lookup qualifiedName eDeclarations

lookupImportIdentifier :: QualifiedName -> Environment -> Maybe ModuleName
lookupImportIdentifier qualifiedName Environment{..}
  = M.lookup qualifiedName eImports

type EnvironmentBuilder
  = Mon.Endo Environment

idAnnot :: JS.P.JSAnnot
idAnnot
  = JS.P.JSAnnot JS.P.tokenPosnEmpty [JS.P.WhiteSpace JS.P.tokenPosnEmpty " "]

pattern JSMethodCall_ meth args <- JS.P.JSMethodCall meth _ args _ _
  where
    JSMethodCall_ meth args
      = JS.P.JSMethodCall meth idAnnot args idAnnot (JS.P.JSSemi idAnnot)

pattern JSExpressionParen_ e <- JS.P.JSExpressionParen _ e _
  where
    JSExpressionParen_ e = JS.P.JSExpressionParen idAnnot e idAnnot

pattern JSFunctionExpression_ name args block <-
  JS.P.JSFunctionExpression _ name _ args _ block

  where
    JSFunctionExpression_ name args block
      = JS.P.JSFunctionExpression idAnnot name idAnnot args idAnnot block

pattern JSLCons_ x xs <- JS.P.JSLCons xs _ x
  where
    JSLCons_ x xs = JS.P.JSLCons xs idAnnot x

pattern JSBlock_ sts <- JS.P.JSBlock _ sts _
  where
    JSBlock_ sts = JS.P.JSBlock idAnnot sts idAnnot

pattern JSMemberSquare_ obj prop <- JS.P.JSMemberSquare obj _ prop _
  where
    JSMemberSquare_ obj prop
      = JS.P.JSMemberSquare obj idAnnot prop idAnnot

pattern JSMemberDot_ obj prop <- JS.P.JSMemberDot obj _ prop
  where
    JSMemberDot_ obj prop
      = JS.P.JSMemberDot obj idAnnot prop

pattern JSIdentName_ name <- JS.P.JSIdentName _ name
  where
    JSIdentName_ name = JS.P.JSIdentName idAnnot name

pattern JSAssignExpression_ lhs rhs <- JS.P.JSAssignExpression lhs _ rhs
  where
    JSAssignExpression_ lhs rhs
      = JS.P.JSAssignExpression lhs (JS.P.JSAssign idAnnot) rhs

pattern JSIdentifier_ name <- JS.P.JSIdentifier _ name
  where
    JSIdentifier_ name = JS.P.JSIdentifier idAnnot name

pattern JSStringLiteral_ s <- JS.P.JSStringLiteral _ s
  where
    JSStringLiteral_ s = JS.P.JSStringLiteral idAnnot s

pattern JSVariable_ vs <- JS.P.JSVariable _ vs _
  where
    JSVariable_ vs
      = JS.P.JSVariable idAnnot vs (JS.P.JSSemi idAnnot)

pattern JSVarInit_ e <- JS.P.JSVarInit _ e
  where
    JSVarInit_ e = JS.P.JSVarInit idAnnot e

pattern JSReturn_ me <- JS.P.JSReturn _ me _
  where
    JSReturn_ me = JS.P.JSReturn idAnnot me (JS.P.JSSemi idAnnot)

pattern JSMemberExpression_ f xs <- JS.P.JSMemberExpression f _ xs _
  where
    JSMemberExpression_ f xs
      = JS.P.JSMemberExpression f idAnnot xs idAnnot

getEnvironment :: JS.P.JSAST -> Environment
getEnvironment
  = flip Mon.appEndo emptyEnvironment . everyTopLevel getEnvironmentQ

getEnvironmentQ :: G.GenericQ (Maybe EnvironmentBuilder)
getEnvironmentQ
  = G.mkQ Nothing go
  where
    go (JSMethodCall_
      (JSExpressionParen_ (JSFunctionExpression_ _
        (JS.P.JSLOne (JSIdentName_ "exports")) modBody))

      (JS.P.JSLOne (JSAssignExpression_
        (JSMemberSquare_ (JSIdentifier_ "PS") (JSStringLiteral_ modName)) _)))

      = Just (getModuleEnvironment modName' modBody)
      where
        modName' = T.pack (stringLiteralString modName)

    go _
      = Nothing

getModuleEnvironment :: T.Text -> JS.P.JSBlock -> EnvironmentBuilder
getModuleEnvironment modName
  = everyTopLevel getModuleEnvironmentQ
  where
    getModuleEnvironmentQ :: G.GenericQ (Maybe EnvironmentBuilder)
    getModuleEnvironmentQ
      = G.mkQ Nothing go
      where
        go (JSVariable_ (JS.P.JSLOne
          (JS.P.JSVarInitExpression
            (JSIdentifier_ declName) (JSVarInit_ declDef))))

          = case declDef of
              JSMemberSquare_ (JSIdentifier_ "PS")
                (JSStringLiteral_ importName) ->

                let importName'
                      = T.pack (stringLiteralString importName)

                    qualifiedName
                      = QualifiedName modName (T.pack (decodeName declName))

                in  Just $ Mon.Endo $ \env ->
                      env { eImports
                              = M.insert qualifiedName importName'
                                  (eImports env)

                          }

              _ ->
                let qualifiedName
                      = QualifiedName modName (T.pack (decodeName declName))

                    decl
                      = Declaration
                          { dQualifiedName    = qualifiedName
                          , dDefinition       = declDef
                          , dOperatorAlias    = maybeOperatorAlias declDef
                          , dPropertyAccessor =
                              maybePropertyAccessor declDef

                          }

                in  Just $ Mon.Endo $ \env ->
                      env { eDeclarations
                              = M.insert qualifiedName decl (eDeclarations env)

                          }

        go _
          = Nothing

type ModuleName
  = T.Text

type DeclarationName
  = T.Text

data QualifiedName
  = QualifiedName
      { qnModule  :: !ModuleName
      , qnName    :: !DeclarationName
      }

  deriving (Eq, Ord, Show)

data Declaration
  = Declaration
      { dQualifiedName    :: !QualifiedName
      , dDefinition       :: !JS.P.JSExpression
      , dOperatorAlias    :: !(Maybe String)
      , dPropertyAccessor :: !(Maybe String)
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
      let env   = getEnvironment ast
          ast'  = withAllDeclarations (inlineOperatorAlias env) ast

          env'  = getEnvironment ast'
          ast'' = withAllDeclarations (inlinePropertyAccessor env') ast'

      writeFile outFile (JS.P.renderToString ast'')

inlineOperatorAlias :: Environment
                    -> QualifiedName
                    -> JS.P.JSExpression
                    -> JS.P.JSExpression

inlineOperatorAlias env qualifiedName@(QualifiedName modName _) e
  = Mb.fromMaybe e $ do
      decl <- lookupDecl qualifiedName env
      opAlias <- dOperatorAlias decl
      let qualifiedAliasName = QualifiedName modName (T.pack opAlias)
      aliasDecl <- lookupDecl qualifiedAliasName env
      pure (dDefinition aliasDecl)

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

              funDecl <- lookupDecl qualifiedFunName env
              propAccessor <- dPropertyAccessor funDecl
              pure (JSMemberDot_ arg (JSIdentifier_ propAccessor))

        go e
          = e

withAllDeclarations :: (QualifiedName -> JS.P.JSExpression -> JS.P.JSExpression)
                    -> JS.P.JSAST
                    -> JS.P.JSAST

withAllDeclarations f
  = atEveryTopLevel withAllDeclarationsM
  where
    withAllDeclarationsM :: G.GenericM Maybe
    withAllDeclarationsM
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
                (withModuleDeclarations (T.pack (stringLiteralString modName))
                  modBody)))

              ae)

        go _
          = Nothing

    withModuleDeclarations :: T.Text -> JS.P.JSBlock -> JS.P.JSBlock
    withModuleDeclarations modName
      = atEveryTopLevel withModuleDeclarationsM
      where
        withModuleDeclarationsM :: G.GenericM Maybe
        withModuleDeclarationsM
          = G.mkMp go
          where
            go (JSVariable_ (JS.P.JSLOne
              (JS.P.JSVarInitExpression
                (JSIdentifier_ declName) (JSVarInit_ declDef))))

              = case declDef of
                  JSMemberSquare_ _ _ ->
                    Nothing

                  _ ->
                    let qualifiedName
                          = QualifiedName modName (T.pack (decodeName declName))

                    in  Just (JSVariable_ (JS.P.JSLOne
                          (JS.P.JSVarInitExpression (JSIdentifier_ declName)
                            (JSVarInit_ (f qualifiedName declDef)))))

            go _
              = Nothing

atEveryTopLevel :: G.GenericM Maybe -> G.GenericT
atEveryTopLevel f x
  = case f x of
      Nothing -> G.gmapT (atEveryTopLevel f) x
      Just y  -> y

everyTopLevel :: Monoid m => G.GenericQ (Maybe m) -> G.GenericQ m
everyTopLevel f x
  = case f x of
      Nothing -> G.gmapQl mappend mempty (everyTopLevel f) x
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

stringLiteralString :: String -> String
stringLiteralString
  = tail . init
