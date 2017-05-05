{-# LANGUAGE RecordWildCards #-}

module Language.PureScript.Optimisation.JavaScript.Environment.Operations where

import Language.PureScript.Optimisation.JavaScript.Environment.Analyses
import Language.PureScript.Optimisation.JavaScript.Environment.Types

import qualified Data.Map.Strict            as M
import qualified Data.Text                  as T
import qualified Language.JavaScript.Parser as JS.P

lookupImportIdentifier :: QualifiedName -> Environment -> Maybe ModuleName
lookupImportIdentifier qualifiedName Environment{..}
  = M.lookup qualifiedName eImports

resolveImportIdentifier :: ModuleName
                        -> String
                        -> Environment
                        -> Maybe ModuleName

resolveImportIdentifier modName objName
  = lookupImportIdentifier (QualifiedName modName (T.pack objName))

lookupDecl :: QualifiedName -> Environment -> Maybe JS.P.JSExpression
lookupDecl qualifiedName Environment{..}
  = dDefinition <$> M.lookup qualifiedName eDeclarations

updateDecl :: QualifiedName -> JS.P.JSExpression -> Environment -> Environment
updateDecl qualifiedName declDef env@Environment{..}
  = env { eDeclarations
            = M.insert qualifiedName decl
                eDeclarations

        }

  where
    decl = mkDeclaration qualifiedName declDef

lookupIsIdentity :: QualifiedName -> Environment -> Bool
lookupIsIdentity qualifiedName Environment{..}
  = maybe False dIsIdentity (M.lookup qualifiedName eDeclarations)

lookupOperatorAlias :: QualifiedName -> Environment -> Maybe String
lookupOperatorAlias qualifiedName Environment{..}
  = M.lookup qualifiedName eDeclarations >>= dOperatorAlias

lookupPropertyAccessor :: QualifiedName -> Environment -> Maybe String
lookupPropertyAccessor qualifiedName Environment{..}
  = M.lookup qualifiedName eDeclarations >>= dPropertyAccessor

lookupPlainConstructor :: QualifiedName -> Environment -> Maybe [String]
lookupPlainConstructor qualifiedName Environment{..}
  = M.lookup qualifiedName eDeclarations >>= dPlainConstructor
