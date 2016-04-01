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
      { eDeclarations :: !(M.Map QualifiedName Declaration)
      , eImports      :: !(M.Map QualifiedName ModuleName)
      }

emptyEnvironment :: Environment
emptyEnvironment
  = Environment
      { eDeclarations = M.empty
      , eImports      = M.empty
      }

type EnvironmentBuilder
  = Mon.Endo Environment

data Declaration
  = Declaration
      { dQualifiedName    :: !QualifiedName
      , dDefinition       :: !JS.P.JSExpression
      , dOperatorAlias    :: !(Maybe String)
      , dPropertyAccessor :: !(Maybe String)
      }

  deriving (Eq, Show)
