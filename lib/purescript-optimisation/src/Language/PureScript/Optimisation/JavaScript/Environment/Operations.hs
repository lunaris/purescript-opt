{-# LANGUAGE RecordWildCards #-}

module Language.PureScript.Optimisation.JavaScript.Environment.Operations where

import Language.PureScript.Optimisation.JavaScript.Environment.Types

import qualified Data.Map.Strict as M

lookupDecl :: QualifiedName -> Environment -> Maybe Declaration
lookupDecl qualifiedName Environment{..}
  = M.lookup qualifiedName eDeclarations

lookupImportIdentifier :: QualifiedName -> Environment -> Maybe ModuleName
lookupImportIdentifier qualifiedName Environment{..}
  = M.lookup qualifiedName eImports
