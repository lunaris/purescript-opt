module Tests.RoundTrip
    ( testRoundTrip
    ) where

import Test.Hspec

import Language.JavaScript.Parser


testRoundTrip :: Spec
testRoundTrip = describe "Roundtrip:" $ do
    it "multi comment" $ do
        testRT "/*a*/\n//foo\nnull"
        testRT "/*a*/x"
        testRT "/*a*/null"
        testRT "/*b*/false"
        testRT "true/*c*/"
        testRT "/*c*/true"
        testRT "/*d*/0x1234fF"
        testRT "/*e*/1.0e4"
        testRT "/*x*/011"
        testRT "/*f*/\"hello\\nworld\""
        testRT "/*g*/'hello\\nworld'"
        testRT "/*h*/this"
        testRT "/*i*//blah/"
        testRT "//j\nthis_"

    it "arrays" $ do
        testRT "/*a*/[/*b*/]"
        testRT "/*a*/[/*b*/,/*c*/]"
        testRT "/*a*/[/*b*/,/*c*/,/*d*/]"
        testRT "/*a*/[/*b/*,/*c*/,/*d*/x/*e*/]"
        testRT "/*a*/[/*b*/,/*c*/,/*d*/x/*e*/]"
        testRT "/*a*/[/*b*/,/*c*/x/*d*/,/*e*/,/*f*/x/*g*/]"
        testRT "/*a*/[/*b*/x/*c*/]"
        testRT "/*a*/[/*b*/x/*c*/,/*d*/]"

    it "object literals" $ do
        testRT "/*a*/{/*b*/}"
        testRT "/*a*/{/*b*/x/*c*/:/*d*/1/*e*/}"
        testRT "x=/*a*/{/*b*/x/*c*/:/*d*/1/*e*/,/*f*/y/*g*/:/*h*/2/*i*/}"
        testRT "x=/*a*/{/*b*/x/*c*/:/*d*/1/*e*/,/*f*/y/*g*/:/*h*/2/*i*/,/*j*/z/*k*/:/*l*/3/*m*/}"
        testRT "a=/*a*/{/*b*/x/*c*/:/*d*/1/*e*/,/*f*/}"

    it "miscellaneous" $ do
        testRT "/*a*/(/*b*/56/*c*/)"
        testRT "/*a*/true/*b*/?/*c*/1/*d*/:/*e*/2"
        testRT "/*a*/x/*b*/||/*c*/y"
        testRT "/*a*/x/*b*/&&/*c*/y"
        testRT "/*a*/x/*b*/|/*c*/y"
        testRT "/*a*/x/*b*/^/*c*/y"
        testRT "/*a*/x/*b*/&/*c*/y"
        testRT "/*a*/x/*b*/==/*c*/y"
        testRT "/*a*/x/*b*/!=/*c*/y"
        testRT "/*a*/x/*b*/===/*c*/y"
        testRT "/*a*/x/*b*/!==/*c*/y"
        testRT "/*a*/x/*b*/</*c*/y"
        testRT "/*a*/x/*b*/>/*c*/y"
        testRT "/*a*/x/*b*/<=/*c*/y"
        testRT "/*a*/x/*b*/>=/*c*/y"
        testRT "/*a*/x /*b*/instance of /*c*/y"
        testRT "/*a*/x/*b*/=/*c*/{/*d*/get/*e*/ foo/*f*/(/*g*/)/*h*/ {/*i*/return/*j*/ 1/*k*/}/*l*/,/*m*/set/*n*/ foo/*o*/(/*p*/a/*q*/) /*r*/{/*s*/x/*t*/=/*u*/a/*v*/}/*w*/}"

    it "statement" $ do
        testRT "if (1) {}"
        testRT "if (1) {} else {}"
        testRT "if (1) x=1; else {}"
        testRT "do {x=1} while (true);"
        testRT "do x=x+1;while(x<4);"
        testRT "while(true);"
        testRT "for(;;);"
        testRT "for(x=1;x<10;x++);"
        testRT "for(var x;;);"
        testRT "for(var x=1;;);"
        testRT "for(var x;y;z){}"
        testRT "for(x in 5){}"
        testRT "for(var x in 5){}"
        testRT "var x=1;"
        testRT "const x=1,y=2;"
        testRT "continue;"
        testRT "continue x;"
        testRT "break;"
        testRT "break x;"
        testRT "return;"
        testRT "return x;"
        testRT "with (x) {};"
        testRT "abc:x=1"
        testRT "switch (x) {}"
        testRT "switch (x) {case 1:break;}"
        testRT "switch (x) {case 0:\ncase 1:break;}"
        testRT "switch (x) {default:break;}"
        testRT "switch (x) {default:\ncase 1:break;}"


testRT :: String -> Expectation
testRT str = str `shouldBe` renderToString (readJs str)

