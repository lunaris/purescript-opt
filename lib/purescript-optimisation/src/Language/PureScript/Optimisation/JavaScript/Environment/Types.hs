module Language.PureScript.Optimisation.JavaScript.Environment.Types where

import qualified Data.Map.Strict            as M
import qualified Data.Monoid                as Mon
import qualified Data.Text                  as T
import qualified Language.JavaScript.Parser as JS.P

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

data Environment
  = Environment
      { eImports            :: !(M.Map QualifiedName ModuleName)
      , eDeclarations       :: !(M.Map QualifiedName JS.P.JSExpression)
      , eOperatorAliases    :: !(M.Map QualifiedName String)
      , ePropertyAccessors  :: !(M.Map QualifiedName String)
      , ePlainConstructors  :: !(M.Map QualifiedName [String])
      }

emptyEnvironment :: Environment
emptyEnvironment
  = Environment
      { eImports            = M.empty
      , eDeclarations       = M.empty
      , eOperatorAliases    = M.empty
      , ePropertyAccessors  = M.empty
      , ePlainConstructors  = M.empty
      }

type EnvironmentBuilder
  = Mon.Endo Environment
