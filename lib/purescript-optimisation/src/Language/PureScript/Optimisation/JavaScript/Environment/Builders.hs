module Language.PureScript.Optimisation.JavaScript.Environment.Builders where

import Language.PureScript.Optimisation.JavaScript.AST
import Language.PureScript.Optimisation.JavaScript.Environment.Analyses
import Language.PureScript.Optimisation.JavaScript.Environment.Types
import Language.PureScript.Optimisation.JavaScript.Schemes

import qualified Data.Generics                  as G
import qualified Data.Map.Strict                as M
import qualified Data.Monoid                    as Mon
import qualified Data.Text                      as T
import qualified Language.JavaScript.Parser.AST as JS.P

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
