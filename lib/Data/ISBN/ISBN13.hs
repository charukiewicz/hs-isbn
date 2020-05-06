{-# LANGUAGE OverloadedStrings #-}

module Data.ISBN.ISBN13
    ( ISBN13
    , validateISBN13
    ) where

import           Data.Text as Text


newtype ISBN13 = ISBN13 Text
    deriving (Show, Eq)

validateISBN13 :: Text -> Either Text ISBN13
validateISBN13 _ = undefined
