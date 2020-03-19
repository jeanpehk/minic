module ParserSpec where

import Parser
import AST

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

spec :: Spec
spec = do

-- identifiers

  describe "ID starting with underscore" $ do
    it "parses whole input" $
      parse identifier "" "_ASdjks492_298" `shouldParse` "_ASdjks492_298"
  describe "ID doesn't start with a number" $ do
    it "should fail on '2'" $
      parse identifier "" `shouldFailOn` "2ident"
  describe "keywords are not identifiers" $ do
    it "should fail on 'return'" $
      parse identifier "" `shouldFailOn` "return"
  describe "identifiers starting with a keyword" $ do
    it "parses as identifier" $
      parse identifier "" "whileIam" `shouldParse` "whileIam"

-- types

  describe "int" $ do
    it "parses as int" $
      parse tpe "" "int" `shouldParse` CInt
  describe "void" $ do
    it "parses as void" $
      parse tpe "" "void" `shouldParse` CVoid

-- expressions

  let ans = Subtr (Add (Mul (IntConst 2) (IntConst 3)) (IntConst 2))
                     (Div (IntConst 33) (IntConst 25))
  describe "expr with arith ops" $ do
    it "should parse with correct precedence" $
      parse parseExpr "" "2*3+2-33/25" `shouldParse` ans

  let ans = Assign (Var "a") (Add (IntConst 3) (IntConst 2))
  describe "expr with assignment" $ do
    it "should parse with assignment as lowest prec" $
      parse parseExpr "" "a=3+2" `shouldParse` ans

  let ans = Lt (Add (IntConst 2) (IntConst 3)) (IntConst 4)
  describe "expr with comp ops" $ do
    it "should parse comparisons as lower prec than arith ops" $
      parse parseExpr "" "2 + 3 < 4" `shouldParse` ans

  let ans = Assign (Var "a") (Assign (Var "b") (Assign (Var "c") (IntConst 4)))
  describe "expr with multiple assignments" $ do
    it "should parse assignments as right assoc" $
      parse parseExpr "" "a=b=c=4" `shouldParse` ans

  let ans = Mul (Add (IntConst 2) (IntConst 2)) (Add (IntConst 3) (IntConst 1))
  describe "expr with parentheses and extra spaces in between and end" $ do
    it "should parse correctly" $
      parse (parseExpr <* eof) "" "(2+2) * (3+1)  " `shouldParse` ans

