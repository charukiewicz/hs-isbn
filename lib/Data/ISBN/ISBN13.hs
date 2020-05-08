{-# LANGUAGE OverloadedStrings #-}

module Data.ISBN.ISBN13
    ( ISBN13
    , validateISBN13
    ) where

import           Control.Monad
import           Data.Text     as Text


-- | Data type for representing ISBN-13 values. Should be created safely using
-- 'validateISBN13', but any string can be coerced into an @ISBN13@ value using
-- 'unsafeToISBN13'.
newtype ISBN13 = ISBN13 Text
    deriving (Show, Eq)

validateISBN13 :: Text -> Either ISBN13ValidationError ISBN13
validateISBN13 input = do
    let inputWithoutHyphens = Text.filter (/= '-') input

    unless (Text.length inputWithoutHyphens == 13) $
        Left InvalidInputLength

    let illegalCharacters = Text.filter (not . isNumericCharacter) inputWithoutHyphens

    unless (Text.length illegalCharacters == 0) $
        Left IllegalCharactersInInput

    pure $ ISBN13 ""


data ISBN13ValidationError
    = InvalidInputLength
    | IllegalCharactersInInput
    deriving (Show, Eq)


-- | Determines whether a character is numeric (e.g. in the range of @0-9@).
isNumericCharacter :: Char -> Bool
isNumericCharacter char = char `elem` ("1234567890" :: String)
