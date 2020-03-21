module CheckerSpec where

import AST
import Checker
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map
import Test.Hspec
import Test.Hspec.Megaparsec

checkRes x = fst $ runChecker x

topLevel x = fst $ (runState . runExceptT) (checkTl x) env
  where
    env = Env { active = ST Map.empty, blocks = [ST Map.empty]}

spec :: Spec
spec = do

  describe "runChecker" $ do
    it "single int with id returns no errors" $
      checkRes (TUnit [(GDecl (Decl CInt "a"))]) `shouldBe` Right ()

  describe "toplevel" $ do
    it "void cannot have an id in a decl" $
      topLevel (GDecl (Decl CVoid "ok")) `shouldBe` Left (SError
                                                         "Void cannot have an identifier")


