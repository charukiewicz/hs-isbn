{-# LANGUAGE OverloadedStrings #-}

module Data.ISBN
    ( -- * Introduction
      -- $introduction

      -- * ISBN-10
      ISBN10
    , validateISBN10
      -- *** Validation Errors
    , ISBN10ValidationError(..)
    , renderISBN10ValidationError

      -- * ISBN-13
    , ISBN13

      -- * Conversion between ISBN-10 and ISBN-13
      -- $conversion
    , convertISBN10toISBN13
    , convertISBN13toISBN10
    ) where

import           Data.ISBN.ISBN10
import           Data.ISBN.ISBN13

import           Data.Text        as Text

-- $introduction
--
-- This library contains tools for validating and working with ISBNs.

------------------------------------

-- $conversion
--
-- ISBN values can be converted from ISBN-10 to ISBN-13 and vise versa.


convertISBN10toISBN13 :: ISBN10 -> ISBN13
convertISBN10toISBN13 isbn10 =
    unsafeToISBN13 $ isbn13Body <> isbn13CheckDigit
      where
        isbn13CheckDigit = Text.singleton . numericValueToISBN13Char $ calculateISBN13CheckDigit isbn13Body
        isbn13Body = "978" <> isbn10Body
        isbn10Body = Text.init $ renderISBN10 isbn10



convertISBN13toISBN10 :: ISBN13 -> Either Text ISBN10
convertISBN13toISBN10 = undefined
