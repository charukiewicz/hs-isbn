{-# LANGUAGE OverloadedStrings #-}

module Data.ISBNSpec (spec) where

import           Data.ISBN
import           Data.ISBN.ISBN10
import           Data.ISBN.ISBN13

import           Data.Text        ( pack )
import           Test.Hspec


spec :: Spec
spec = do
    describe "Testing ISBN validation" $ do
        let test_validateISBN isbn expecting =
                it (concat
                        [ "can "
                        , case expecting of Right _ -> "validate"; Left _ -> "reject  "
                        , " '"
                        , isbn
                        , "'"
                        , take (18 - length isbn) $ cycle " "
                        , "as '"
                        , show expecting
                        , "'"
                        ]) $
                    validateISBN (pack isbn) `shouldBe` expecting

        test_validateISBN "0345816021"        (Right (unsafeToISBN10 "0345816021"))
        test_validateISBN "0-807-01429-X"     (Right (unsafeToISBN10 "080701429X"))
        test_validateISBN "9780807014295"     (Right (unsafeToISBN13 "9780807014295"))
        test_validateISBN "978-0-306-40615-7" (Right (unsafeToISBN13 "9780306406157"))
        test_validateISBN "979-10-95546-00-9" (Right (unsafeToISBN13 "9791095546009"))
        test_validateISBN "0-345-816"         (Left InvalidISBNInputLength)
        test_validateISBN "X-345-81602-1"     (Left IllegalCharactersInISBN10Body)
        test_validateISBN "0-345-81602-B"     (Left IllegalCharacterAsISBN10CheckDigit)
        test_validateISBN "0-345-81602-3"     (Left InvalidISBN10CheckDigit)
        test_validateISBN "00000000000000"    (Left InvalidISBNInputLength)
        test_validateISBN "9780807014299"     (Left InvalidISBN13CheckDigit)
        test_validateISBN "0X00000000000"     (Left IllegalCharactersInISBN13Input)

    describe "Testing ISBN-10 to ISBN-13 conversion" $ do
        let test_convertISBN10toISBN13 isbn10 expecting =
                it ("can convert (" ++ show isbn10 ++ ") to (" ++ show expecting ++ ")") $
                    convertISBN10toISBN13 isbn10 `shouldBe` expecting

        test_convertISBN10toISBN13 (unsafeToISBN10 "0060899220") (unsafeToISBN13 "9780060899226")
        test_convertISBN10toISBN13 (unsafeToISBN10 "1519150245") (unsafeToISBN13 "9781519150240")

    describe "Testing ISBN-13 to ISBN-10 conversion" $ do
        let test_convertISBN13toISBN10 isbn13 expecting =
                it ("can convert (" ++ show isbn13 ++ ") to (" ++ show expecting ++ ")") $
                    convertISBN13toISBN10 isbn13 `shouldBe` expecting

        test_convertISBN13toISBN10 (unsafeToISBN13 "9780060899226") (Just $ unsafeToISBN10 "0060899220")
        test_convertISBN13toISBN10 (unsafeToISBN13 "9781519150240") (Just $ unsafeToISBN10 "1519150245")
        test_convertISBN13toISBN10 (unsafeToISBN13 "2222222222222") Nothing
