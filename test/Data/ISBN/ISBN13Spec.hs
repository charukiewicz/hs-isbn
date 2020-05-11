{-# LANGUAGE OverloadedStrings #-}

module Data.ISBN.ISBN13Spec (spec) where

import           Data.ISBN.ISBN13

import           Data.List        ( concat )
import           Data.Text        ( pack )
import           Test.Hspec


spec :: Spec
spec = do

    describe "Testing ISBN-13 check digit calculation" $ do
        let test_calculateISBN13CheckDigitValue isbn13BodyString expecting =
                it ("can calculate check digit for '" ++ isbn13BodyString ++ "' should be '" ++ show expecting ++ "'") $
                    calculateISBN13CheckDigitValue (pack isbn13BodyString) `shouldBe` expecting

        test_calculateISBN13CheckDigitValue "978030640615" 7
        test_calculateISBN13CheckDigitValue "978034581602" 3
        test_calculateISBN13CheckDigitValue "978080701429" 5

    describe "Testing ISBN-13 check digit confirmation" $ do
        let test_confirmISBN13CheckDigit isbn13string expecting =
              it ("can " ++ (if expecting then "confirm" else "reject ") ++ " '" ++ isbn13string ++ "'") $
                  confirmISBN13CheckDigit (pack isbn13string) `shouldBe` expecting

        test_confirmISBN13CheckDigit "9780306406157" True
        test_confirmISBN13CheckDigit "9780345816023" True
        test_confirmISBN13CheckDigit "9780345816029" False
        test_confirmISBN13CheckDigit "9780807014295" True

    describe "Testing ISBN-13 validation with errors" $ do
        let test_validateISBN13 isbn13 expecting =
                it (concat
                        [ "can "
                        , case expecting of Right _ -> "validate"; Left _ -> "reject  "
                        , " '"
                        , isbn13
                        , "'"
                        , take (18 - length isbn13) $ cycle " "
                        , "as '"
                        , show expecting
                        , "'"
                        ]) $
                    validateISBN13 (pack isbn13) `shouldBe` expecting

        test_validateISBN13 "9780345816023"     (Right $ unsafeToISBN13 "9780345816023")
        test_validateISBN13 "9780345816029"     (Left InvalidISBN13CheckDigit)
        test_validateISBN13 "9780807014295"     (Right $ unsafeToISBN13 "9780807014295")
        test_validateISBN13 "9780807014299"     (Left InvalidISBN13CheckDigit)
        test_validateISBN13 "978-0-306-40615-7" (Right $ unsafeToISBN13 "9780306406157")
        test_validateISBN13 "9780306406157"     (Right $ unsafeToISBN13 "9780306406157")
        test_validateISBN13 "00000000000000"    (Left InvalidISBN13InputLength)
        test_validateISBN13 "0X00000000000"     (Left IllegalCharactersInISBN13Input)
