module CheckerSpec where

import Checker
import AST

import Test.Hspec
import Test.Hspec.Megaparsec

checkRes x = fst $ runChecker x

spec :: Spec
spec = do

  describe "runChecker" $ do
    it "single int with id returns no errors" $
      checkRes (TUnit [(GDecl (Decl CInt "a"))]) `shouldBe` Right ()

