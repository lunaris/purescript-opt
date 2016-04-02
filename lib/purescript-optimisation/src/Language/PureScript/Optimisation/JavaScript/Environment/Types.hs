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
      { eImports      :: !(M.Map QualifiedName ModuleName)
      , eDeclarations :: !(M.Map QualifiedName Declaration)
      }

emptyEnvironment :: Environment
emptyEnvironment
  = Environment
      { eImports      = M.empty
      , eDeclarations = M.empty
      }

type EnvironmentBuilder
  = Mon.Endo Environment

data Declaration
  = Declaration
      { dQualifiedName    :: !QualifiedName
      , dDefinition       :: !JS.P.JSExpression
      , dIsIdentity       :: !Bool
      , dOperatorAlias    :: !(Maybe String)
      , dPropertyAccessor :: !(Maybe String)
      , dPlainConstructor :: !(Maybe [String])
      }

  deriving (Eq, Show)
