module CheckerSpec where

import AST
import Checker
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map
import Test.Hspec
import Test.Hspec.Megaparsec

-- helpers for running tests
checkRes x = fst $ runChecker x
exprTest x = fst $ (runState . runExceptT) (checkExpr x) env

env = Env { active = ST Map.empty, blocks = [ST Map.empty]}

spec :: Spec
spec = do

-- translation unit
  describe "TUnit" $ do
    describe "simple Decl" $ do
      it "single int with id returns no errors" $
        checkRes (TUnit [(GDecl (Decl CInt "a"))]) `shouldBe` Right ()
    describe "Void Decl" $ do
      it "void cannot have an id in a decl" $
        checkRes (TUnit [(GDecl (Decl CVoid "ok"))])
        `shouldBe` Left (SError "Void cannot have an identifier")

    describe "Functions" $ do
      it "void function is allowed" $
        checkRes (TUnit [FDef (Func CVoid "hey" [] (Block []))]) `shouldBe` Right ()
      it "Functions with same not allowed" $
        checkRes (TUnit [FDef (Func CVoid "hey" [] (Block []))
                       , FDef (Func CVoid "hey" [] (Block []))])
       `shouldBe` Left (TError "id: 'hey' already declared")

    describe "DeclsAndFuncs" $ do
      it "Function can't have same name as already declared var" $
        checkRes (TUnit [GDecl (Decl CInt "already")
                       , FDef (Func CVoid "already" [] (Block []))])
        `shouldBe` Left (TError "id: 'already' already declared")

-- expressions
  describe "Expr" $ do
    describe "Binops" $ do
      describe "Add" $
        it "two ints are ok" $
          exprTest (Add (IntConst 5) (IntConst 0)) `shouldBe` Right CInt

