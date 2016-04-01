module Language.PureScript.Optimisation.JavaScript.Inliner where

import Language.PureScript.Optimisation.JavaScript.AST
import Language.PureScript.Optimisation.JavaScript.Environment

import qualified Data.Generics                  as G
import qualified Data.Maybe                     as Mb
import qualified Data.Text                      as T
import qualified Language.JavaScript.Parser     as JS.P
import qualified Language.JavaScript.Parser.AST as JS.P

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
