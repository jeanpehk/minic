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
    it "doesn't allow alphabetic chars after 't'" $
      parse tpe "" `shouldFailOn` "intskjd"
    it "doesn't allow any underscore after 't'" $
      parse tpe "" `shouldFailOn` "int_"
  describe "void" $ do
    it "parses as void" $
      parse tpe "" "void" `shouldParse` CVoid
    it "doesn't allow num chars after 'd'" $
      parse tpe "" `shouldFailOn` "void214d"
    it "doesn't allow any underscore after 'd'" $
      parse tpe "" `shouldFailOn` "void_"
  describe "char" $ do
    it "parses as char" $
      parse tpe "" "char" `shouldParse` CChar
  describe "pointer int" $ do
    it "parses" $
      parse tpe "" "int *" `shouldParse` Pntr (CInt)
  describe "pointer to pointer void" $ do
    it "parses" $
      parse tpe "" "void * *" `shouldParse` Pntr (Pntr CVoid)

-- expressions

  let ans = BinOp Subtr (BinOp Add (BinOp Mul (IntConst 2) (IntConst 3)) (IntConst 2))
                     (BinOp Div (IntConst 33) (IntConst 25))
  describe "expr with arith ops" $ do
    it "should parse with correct precedence" $
      parse expr "" "2*3+2-33/25" `shouldParse` ans

  let ans = Assign "a" (BinOp Add (IntConst 3) (IntConst 2))
  describe "expr with assignment" $ do
    it "should parse with assignment as lowest prec" $
      parse expr "" "a=3+2" `shouldParse` ans

  let ans = CharConst 'u'
  describe "character constant" $ do
    it "should parse" $
      parse expr "" "'u'" `shouldParse` ans

  let ans = Assign "abcd" (CharConst 'k')
  describe "assignment to char" $ do
    it "should parse" $
      parse expr "" "abcd='k'" `shouldParse` ans

  let ans = BinOp Lt (BinOp Add (IntConst 2) (IntConst 3)) (IntConst 4)
  describe "expr with comp ops" $ do
    it "should parse comparisons as lower prec than arith ops" $
      parse expr "" "2 + 3 < 4" `shouldParse` ans

  let ans = Assign "a" (Assign "b" (Assign "c" (IntConst 4)))
  describe "expr with multiple assignments" $ do
    it "should parse assignments as right assoc" $
      parse expr "" "a=b=c=4" `shouldParse` ans

  let ans = BinOp Mul (BinOp Add (IntConst 2) (IntConst 2)) (BinOp Add (IntConst 3) (IntConst 1))
  describe "expr with parentheses and extra spaces in between and end" $ do
    it "should parse correctly" $
      parse (expr <* eof) "" "(2+2) * (3+1)  " `shouldParse` ans

  describe "longer munch rule: equality over assignment" $ do
    it "should parse as eq expr" $
      parse expr  "" "a==3" `shouldParse` BinOp Eq (Var "a") (IntConst 3)

  let ans = FCall "noArgsFunc" []
  describe "Function call without args" $ do
    it "should parse" $
      parse expr "" "noArgsFunc() ;" `shouldParse` ans

  let ans = FCall "ArgsFunc" [IntConst 3, IntConst 2]
  describe "Function call with int const args" $ do
    it "should parse" $
      parse expr "" "ArgsFunc(3, 2) ;" `shouldParse` ans

-- statements

  let ans = While (BinOp Lt (Var "myvar") (IntConst 3)) (ExprStmt (Assign "myvar" (IntConst 3)))
  describe "while stmt" $ do
    it "should parse with LT expr and ExprStmt" $
      parse stmt "" "while (myvar < 3) myvar = 3;" `shouldParse` ans

  let ans = While (BinOp Eq (IntConst 2) (IntConst 3)) Null
  describe "while with random spaces and a null stmt" $ do
    it "should parse with EQ expr and ExprStmt" $
      parse stmt "" "while  (  2 ==   3  ) ;" `shouldParse` ans

  let ans = IfElse (BinOp Eq (Var "abcd") (IntConst 5))
                   (ExprStmt (Assign "abcd" (IntConst 6)))
                   (ExprStmt (Assign "abcd" (IntConst 7)))
  describe "if else statement" $ do
    it "should parse with Eq expr and assignment statements" $
      parse stmt "" "if (abcd == 5) abcd = 6; else abcd = 7;" `shouldParse` ans

  let ans = Return (Just (IntConst 2))
  describe "return int const" $ do
    it "should parse as Just" $
      parse stmt "" "return 2;" `shouldParse` ans

  let ans = Return Nothing
  describe "return nothing" $ do
    it "should parse as Nothing" $
      parse stmt "" "return ;" `shouldParse` ans

-- translation unit

  describe "translation unit" $ do
    it "doesn't allow junk as input" $
      parse tunit "" `shouldFailOn` "djklkj"

  let ans = TUnit [GDecl (Decl CInt "a"), GDecl (Decl CInt "b")]
  describe "translation unit with decls" $ do
    it "should parse two variable declarations" $
      parse tunit "" "int a; int b;" `shouldParse` ans

  let ans = TUnit [GDecl (Decl CInt "ok"), FDef (Func CVoid "hello" [] (Block []))]
  describe "translation unit with decl and function" $ do
    it "should parse as `int ok` declaration and `void hello` function" $
      parse tunit "" "int ok; void hello() {}" `shouldParse` ans

  let ans = TUnit [FDef (Func CVoid "Alright" [] (Block [])), GDecl (Decl CInt "right"), FDef (Func CInt "main" [] (Block []))]
  describe "translation unit with function, decl, function" $ do
    it "should parse as `void Alright` function, `int right` decl and `int main` function" $
      parse tunit "" "void Alright() {} int right; int main(){}" `shouldParse` ans

