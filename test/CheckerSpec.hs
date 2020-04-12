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

env = Env { active = ST Map.empty, blocks = [], used = []}

spec :: Spec
spec = do

-- translation unit
  describe "TUnit" $ do
    describe "Decls" $ do
      it "single int with id returns no errors" $
        checkRes (TUnit [(GDecl (Decl CInt "a"))]) `shouldBe` Right ()
      it "void cannot have an id in a decl" $
        checkRes (TUnit [(GDecl (Decl CVoid "ok"))])
        `shouldBe` Left (SError "Void cannot have an identifier")
      it "Cannot have declarations with same name" $
        checkRes (TUnit [(GDecl (Decl CInt "ok"))
                      , (GDecl (Decl CInt "ok"))])
        `shouldBe` Left (TError "id: 'ok' already declared")

    describe "Functions" $ do
      it "void function is allowed" $
        checkRes (TUnit [FDef (Func CVoid "hey" [] (Block []))]) `shouldBe` Right ()
      it "Functions with same not allowed" $
        checkRes (TUnit [FDef (Func CVoid "hey" [] (Block []))
                       , FDef (Func CVoid "hey" [] (Block []))])
       `shouldBe` Left (TError "id: 'hey' already declared")
      it "Function cant have params with same name" $
        checkRes (TUnit [FDef (Func CVoid "func"
                              [Param CInt "a", Param CInt "a"] (Block []))])
        `shouldBe` Left (TError (dError "a"))
      it "Function can have same name as its param" $
        checkRes (TUnit [FDef (Func CVoid "test" [Param CInt "test"] (Block []))])
        `shouldBe` Right ()
      it "Function with single void param is ok" $
        checkRes (TUnit [FDef (Func CVoid "test" [ParamNoId CVoid] (Block []))])
        `shouldBe` Right ()
      it "Function with named void param is not correct" $
        checkRes (TUnit [FDef (Func CVoid "test" [Param CVoid "ok"] (Block []))])
        `shouldBe` Left (TError ("Void param cannot have an identifier"))
      it "Function with non-void no id param is not correct" $
        checkRes (TUnit [FDef (Func CVoid "test" [ParamNoId CInt] (Block []))])
        `shouldBe` Left (TError ("Non-void param needs an identifier"))

    describe "Blocks" $ do
      it "decl in a block can have same name as global decl" $
        checkRes (TUnit [ GDecl (Decl CInt "a")
                       ,  FDef  (Func CVoid "f" []
                                (Block [Left (Decl CInt "a")]))])
        `shouldBe` Right ()
      it "decls in the same block can't have same names" $
        checkRes (TUnit [FDef (Func CVoid "f" []
                                (Block [Left (Decl CInt "a")
                                      , Right Null
                                      , Left (Decl CInt "a")]))])
        `shouldBe` Left (TError (dError "a"))
      it "decl in the primary func block can't have same name as func param" $
        checkRes (TUnit [FDef (Func CVoid "_384kK__" [Param CInt "two"]
                                (Block [Left (Decl CInt "one")
                                      , Left (Decl CInt "two")]))])
        `shouldBe` Left (TError (dError "two"))
      it "decl in a function block can have the same name as the func itself" $
        checkRes (TUnit [FDef (Func CVoid "MINE" []
                                (Block [Left (Decl CInt "MINE")]))])
        `shouldBe` Right ()

    describe "DeclsAndFuncs" $ do
      it "Function can't have same name as already declared var" $
        checkRes (TUnit [GDecl (Decl CInt "already")
                       , FDef (Func CVoid "already" [] (Block []))])
        `shouldBe` Left (TError "id: 'already' already declared")
      it "Function can have param with same name as an earlier decl" $
        checkRes (TUnit [GDecl (Decl CInt "test")
                       , FDef (Func CVoid "nnN_32c" [Param CInt "test"] (Block []))])
        `shouldBe` Right ()

-- expressions
  describe "Expr" $ do
    describe "Binops" $ do
      describe "Add" $
        it "two ints are ok" $
          exprTest (Add (IntConst 5) (IntConst 0)) `shouldBe` Right CInt

    describe "Binops" $ do
      describe "Mul" $
        it "int and char are ok" $
          exprTest (Mul (IntConst 5) (CharConst 'a')) `shouldBe` Right CInt

