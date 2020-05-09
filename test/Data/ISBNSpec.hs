{-# LANGUAGE OverloadedStrings #-}

module Data.ISBNSpec (spec) where

import           Data.ISBN
import           Data.ISBN.ISBN10
import           Data.ISBN.ISBN13

import           Data.Text        ( pack )
import           Test.Hspec


spec :: Spec
spec = do
    describe "ISBN conversion" $ do
        it "can convert ISBN-10 to ISBN-13" $ do
            convertISBN10toISBN13 (unsafeToISBN10 "0060899220") `shouldBe` (unsafeToISBN13 "9780060899226")
