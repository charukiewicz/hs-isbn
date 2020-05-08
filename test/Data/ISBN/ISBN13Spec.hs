{-# LANGUAGE OverloadedStrings #-}

module Data.ISBN.ISBN13Spec (spec) where

import           Data.ISBN.ISBN13

import           Data.Text        ( pack )
import           Test.Hspec


spec :: Spec
spec = do
    describe "Checking for valid ISBN 13 check digit characters" $ do
        it "can compare" $ do
            True `shouldBe` True
