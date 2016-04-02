module Language.PureScript.Optimisation.JavaScript.Inliner where

import Language.PureScript.Optimisation.JavaScript.Inliner.Pipeline

import qualified Language.JavaScript.Parser as JS.P

test :: FilePath -> FilePath -> IO ()
test inFile outFile
  = do
      ast <- JS.P.parseFile inFile
      let ast' = applyInlinings `untilStable` ast
      writeFile outFile (JS.P.renderToString ast')

untilStable :: Eq a => (a -> a) -> a -> a
untilStable f x
  = let x' = f x
    in  if x == x' then x else untilStable f x'
