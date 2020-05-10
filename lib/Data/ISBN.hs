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

import           Control.Monad
import           Data.Text        as Text

-- $introduction
--
-- This library contains tools for validating and working with ISBNs.

------------------------------------

-- $conversion
--
-- ISBN values can be converted from ISBN-10 to ISBN-13 and vise versa.


-- | Convert an ISBN-10 to an ISBN-13. Since all ISBN-10s can be converted to
-- ISBN-13s, this operation cannot fail.
--
-- /Example:/
--
-- @
-- convertISBN10toISBN13 (ISBN10 "0060899220") == ISBN13 "9780060899226"
-- @
convertISBN10toISBN13 :: ISBN10 -> ISBN13
convertISBN10toISBN13 isbn10 =
    unsafeToISBN13 $ isbn13Body <> isbn13CheckDigit
      where
        isbn13CheckDigit = Text.singleton . numericValueToISBN13Char $ calculateISBN13CheckDigitValue isbn13Body
        isbn13Body = "978" <> isbn10Body
        isbn10Body = Text.init $ renderISBN10 isbn10


-- | Convert an ISBN-13 to an ISBN-10. Since only ISBN-13s starting with '978'
-- can be converted, this operation may fail.
convertISBN13toISBN10 :: ISBN13 -> Either Text ISBN10
convertISBN13toISBN10 isbn13 = do
    let isbn13Text = renderISBN13 isbn13
    unless ("978" `isPrefixOf` isbn13Text) $
        Left "ISBN-13 does not begin with '978'"

    let isbn10Body = Text.init $ Text.drop 3 isbn13Text
        isbn10CheckDigit = Text.singleton . numericValueToISBN10Char $ calculateISBN10CheckDigitValue isbn10Body

    pure $ unsafeToISBN10 $ isbn10Body <> isbn10CheckDigit
