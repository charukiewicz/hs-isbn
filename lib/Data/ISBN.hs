{-# LANGUAGE OverloadedStrings #-}

module Data.ISBN
    ( module Data.ISBN.ISBN10
    , module Data.ISBN.ISBN13
    , convertISBN10toISBN13
    , convertISBN13toISBN10
    ) where

import           Data.ISBN.ISBN10
import           Data.ISBN.ISBN13

import           Data.Text        as Text

convertISBN10toISBN13 :: ISBN10 -> ISBN13
convertISBN10toISBN13 = undefined

convertISBN13toISBN10 :: ISBN13 -> Either Text ISBN10
convertISBN13toISBN10 = undefined
