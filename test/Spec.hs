{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Data.ISBN
import           Data.Text        ( pack )

import           Data.Traversable

import           Test.Hspec


main :: IO ()
main = hspec $ do
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
