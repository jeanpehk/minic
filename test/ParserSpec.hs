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

