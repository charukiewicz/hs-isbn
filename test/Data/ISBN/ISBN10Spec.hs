{-# LANGUAGE OverloadedStrings #-}

module Data.ISBN.ISBN10Spec (spec) where

import           Data.ISBN.ISBN10

import           Data.Text        ( pack )
import           Test.Hspec


spec :: Spec
spec = do
    describe "Checking for valid ISBN 10 check digit characters" $ do
        let testValidChar char expecting =
              it ("can " ++ (if expecting then "accept" else "reject") ++ " '" ++ [char] ++ "'") $
                  isValidISBN10CheckDigit char `shouldBe` expecting

        testValidChar 'X' True
        testValidChar '1' True
        testValidChar '2' True
        testValidChar '3' True
        testValidChar '4' True
        testValidChar '5' True
        testValidChar '6' True
        testValidChar '7' True
        testValidChar '8' True
        testValidChar '9' True
        testValidChar '0' True
        testValidChar 'C' False
        testValidChar 'F' False
        testValidChar 'a' False
        testValidChar 'x' False
        testValidChar '-' False
        testValidChar '_' False
        testValidChar '*' False
        testValidChar '!' False
        testValidChar ' ' False

    describe "Checking ISBN 10 check digit confirmation" $ do
        let testCheckDigit isbn10string expecting =
              it ("can " ++ (if expecting then "confirm" else "reject ") ++ " '" ++ isbn10string ++ "'") $
                  confirmISBN10CheckDigit (pack isbn10string) `shouldBe` expecting

        testCheckDigit "0306406152" True
        testCheckDigit "030640615X" False
        testCheckDigit "0345816021" True
        testCheckDigit "080701429X" True
        testCheckDigit "0060899220" True
        testCheckDigit "2222222222" True
        testCheckDigit "4444444444" True
        testCheckDigit "9999999999" True
        testCheckDigit "999999999X" False

    describe "Validating possible ISBN-10s" $ do
        let testISBN10 isbn10 expecting =
                it ("can " ++ (case expecting of Right _ -> "validate"; Left _ -> "reject  ") ++ " '" ++ isbn10 ++ "'") $
                    validateISBN10 (pack isbn10) `shouldBe` expecting

        testISBN10 "0-345-81602-1" (Right $ unsafeToISBN10 "0345816021")
        testISBN10 "0345816021"    (Right $ unsafeToISBN10 "0345816021")
        testISBN10 "080701429X"    (Right $ unsafeToISBN10 "080701429X")
        testISBN10 "0-345-81602-0" (Left InvalidISBN10CheckDigit)
        testISBN10 "0-345-X1602-0" (Left IllegalCharactersInISBN10Body)
        testISBN10 "A-345-X1602-0" (Left IllegalCharactersInISBN10Body)
        testISBN10 "A2345-X1602-0" (Left InvalidISBN10InputLength)
        testISBN10 "345-81602-1"   (Left InvalidISBN10InputLength)
