{-# LANGUAGE LambdaCase #-}

module Language.PureScript.Optimisation.JavaScript.AST.Strings where

import qualified Data.Char as Ch

decodeName :: String -> String
decodeName []
  = []

decodeName ('$' : '$' : jsReserved)
  = jsReserved

decodeName ('$' : cs)
  = decodeSymbol nextSym ++ decodeName rest
  where
    (nextSym, rest) = break (\c -> Ch.isUpper c || c == '$') cs

decodeName s
  = untilNext ++ decodeName rest
  where
    (untilNext, rest) = break (== '$') s

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
