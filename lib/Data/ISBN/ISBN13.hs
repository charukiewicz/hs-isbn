{-# LANGUAGE OverloadedStrings #-}

module Data.ISBN.ISBN13
    ( ISBN13
    , validateISBN13
    , renderISBN13
    , renderISBN13ValidationError
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
        Left InvalidISBN13InputLength

    let illegalCharacters = Text.filter (not . isNumericCharacter) inputWithoutHyphens

    unless (Text.length illegalCharacters == 0) $
        Left IllegalCharactersInISBN13Input

    unless (confirmISBN13CheckDigit inputWithoutHyphens) $
        Left InvalidISBN13CheckDigit

    pure $ ISBN13 inputWithoutHyphens


renderISBN13 :: ISBN13 -> Text
renderISBN13 (ISBN13 isbn13string) = isbn13string


-- | Possible validation errors resulting from ISBN-13 validation.
data ISBN13ValidationError
    = InvalidISBN13InputLength       -- ^ The length of the input string is not 13 characters, not counting hyphens
    | IllegalCharactersInISBN13Input -- ^ The ISBN-13 input contains non-numeric characters
    | InvalidISBN13CheckDigit        -- ^ The check digit is not valid for the given ISBN-13
    deriving (Show, Eq)

-- | Convert an 'ISBN10ValidationError' into a human-friendly error message.
renderISBN13ValidationError :: ISBN13ValidationError -> Text
renderISBN13ValidationError validationError =
    case validationError of
        InvalidISBN13InputLength ->
            "An ISBN-13 must be 13 characters, not counting hyphens"

        IllegalCharactersInISBN13Input ->
            "Every non-hyphen character of an ISBN-13 must be a number"

        InvalidISBN13CheckDigit ->
            "The supplied ISBN-13 is not valid"


-- | Determines whether a character is numeric (e.g. in the range of @0-9@).
isNumericCharacter :: Char -> Bool
isNumericCharacter char = char `elem` ("1234567890" :: String)

confirmISBN13CheckDigit :: Text -> Bool
confirmISBN13CheckDigit isbn13 =
    calculateISBN13CheckDigitValue (Text.init isbn13) == isbn13CharToNumericValue (Text.last isbn13)

calculateISBN13CheckDigitValue :: Text -> Int
calculateISBN13CheckDigitValue input =
    go 1 (unpack input) 0
      where
        go w charList acc =
            case charList of
              [] -> (10 - (acc `mod` 10)) `mod` 10
              c:clist -> go ((w + 2) `mod` 4) clist (acc + w * isbn13CharToNumericValue c)


-- | Converts an ISBN-13 character to a numeric value. Valid input characters
-- include @0-9@ as well as @X@.
isbn13CharToNumericValue :: Char -> Int
isbn13CharToNumericValue = digitToInt

-- | Converts a numeric value to an ISBN-13 character. Valid input values
-- are the numbers from 0 to 10.
numericValueToISBN13Char :: Int -> Char
numericValueToISBN13Char c = Text.head $ pack $ show c



-- | Allows for the generation of 'ISBN13' values without any validation.
unsafeToISBN13 :: Text -> ISBN13
unsafeToISBN13 = ISBN13
