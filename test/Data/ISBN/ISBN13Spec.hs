{-# LANGUAGE OverloadedStrings #-}

module Data.ISBN.ISBN13Spec (spec) where

import           Data.ISBN.ISBN13

import           Data.Text        ( pack )
import           Test.Hspec


spec :: Spec
spec = do
    describe "Checking for valid ISBN 13 check digit characters" $ do
        it "can validate ISBN13" $ do
            validateISBN13 "9780345816023"     `shouldBe` (Right $ unsafeToISBN13 "9780345816023")
            validateISBN13 "9780345816029"     `shouldBe` Left InvalidCheckDigit
            validateISBN13 "9780807014295"     `shouldBe` (Right $ unsafeToISBN13 "9780807014295")
            validateISBN13 "9780807014299"     `shouldBe` Left InvalidCheckDigit
            validateISBN13 "978-0-306-40615-7" `shouldBe` (Right $ unsafeToISBN13 "9780306406157")
            validateISBN13 "9780306406157"     `shouldBe` (Right $ unsafeToISBN13 "9780306406157")
            validateISBN13 "00000000000000"    `shouldBe` Left InvalidInputLength
            validateISBN13 "0X00000000000"     `shouldBe` Left IllegalCharactersInInput
    describe "Confirming ISBN-13 check digits" $ do
        it "can confirm ISBN-13 has a valid check digit" $ do
            confirmISBN13CheckDigit "9780306406157" `shouldBe` True
            confirmISBN13CheckDigit "9780345816023" `shouldBe` True
            confirmISBN13CheckDigit "9780345816029" `shouldBe` False
            confirmISBN13CheckDigit "9780807014295" `shouldBe` True

    describe "Calculating ISBN-13 check digits" $ do
        it "can calculate ISBN13 check digit" $ do
            calculateISBN13CheckDigitValue "978030640615" `shouldBe` 7
            calculateISBN13CheckDigitValue "978034581602" `shouldBe` 3
            calculateISBN13CheckDigitValue "978080701429" `shouldBe` 5
