{-# LANGUAGE OverloadedStrings #-}

module Data.ISBNSpec (spec) where

import           Data.ISBN
import           Data.ISBN.ISBN10
import           Data.ISBN.ISBN13

import           Data.Text        ( pack )
import           Test.Hspec


spec :: Spec
spec = do
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
