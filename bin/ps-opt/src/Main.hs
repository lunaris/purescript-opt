module Main (main) where

import Language.PureScript.Optimisation.JavaScript.Inliner as PS.O.J.I

main :: IO ()
main
  = do
      PS.O.J.I.test "/tmp/app.standalone.js" "/tmp/app.standalone.out.js"
