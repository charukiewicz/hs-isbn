{-# LANGUAGE OverloadedStrings #-}

module Data.ISBN.ISBN13
    ( ISBN13
    , validateISBN13
    , renderISBN13
    , ISBN13ValidationError(..)
    , confirmISBN13CheckDigit
    , calculateISBN13CheckDigitValue
    , numericValueToISBN13Char
    , unsafeToISBN13
    ) where

import           Control.Monad
import           Data.Char
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

    unless (confirmISBN13CheckDigit inputWithoutHyphens) $
        Left InvalidCheckDigit

    pure $ ISBN13 inputWithoutHyphens


renderISBN13 :: ISBN13 -> Text
renderISBN13 (ISBN13 isbn13string) = isbn13string


data ISBN13ValidationError
    = InvalidInputLength
    | IllegalCharactersInInput
    | InvalidCheckDigit
    deriving (Show, Eq)


-- | Determines whether a character is numeric (e.g. in the range of @0-9@).
isNumericCharacter :: Char -> Bool
isNumericCharacter char = char `elem` ("1234567890" :: String)

confirmISBN13CheckDigit :: Text -> Bool
confirmISBN13CheckDigit isbn13 =
    (calculateISBN13CheckDigitValue $ Text.init isbn13) == (isbn13CharToNumericValue $ Text.last isbn13)

calculateISBN13CheckDigitValue :: Text -> Int
calculateISBN13CheckDigitValue input =
    go 1 (unpack input) 0
      where
        go w charList acc =
            case charList of
              [] -> (10 - (acc `mod` 10)) `mod` 10
              c:clist -> go ((w + 2) `mod` 4) clist (acc + w * (isbn13CharToNumericValue c))


-- | Converts an ISBN-13 character to a numeric value. Valid input characters
-- include @0-9@ as well as @X@.
isbn13CharToNumericValue :: Char -> Int
isbn13CharToNumericValue c = digitToInt c

-- | Converts a numeric value to an ISBN-13 character. Valid input values
-- are the numbers from 0 to 10.
numericValueToISBN13Char :: Int -> Char
numericValueToISBN13Char c = Text.head $ pack $ show c




unsafeToISBN13 :: Text -> ISBN13
unsafeToISBN13 = ISBN13
