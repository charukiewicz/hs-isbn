{-# LANGUAGE OverloadedStrings #-}

module Data.ISBN.ISBN10
    ( ISBN10
    , validateISBN10
      -- * Validation Errors
    , ISBN10ValidationError(..)
    , renderISBN10ValidationError
      -- * Helpers
    , confirmISBN10CheckDigit
    , calculateISBN10CheckDigitValue
    , isbn10CharToNumericValue
    , numericValueToISBN10Char
    , isValidISBN10CheckDigit
    , isNumericCharacter
      -- * Unsafe Coercion
    , unsafeToISBN10
    ) where

import           Control.Monad
import           Data.Char
import           Data.Text     as Text


-- | Data type for representing ISBN-10 values. Should be created safely using
-- 'validateISBN10', but any string can be coerced into an @ISBN10@ value using
-- 'unsafeToISBN10'.
newtype ISBN10 = ISBN10 Text
    deriving (Show, Eq)


-- | Used to safely create ISBN-10 values represented by the 'ISBN10' data type.
-- Assumes that the 'Data.Text.Text' input is an ISBN-10 string, either with or
-- without hyphens.
--
-- Will return either a validated ISBN-10 or an 'ISBN10ValidationError', which can be
-- rendered as a descriptive string using 'renderISBN10ValidationError'.
--
-- /Examples:/
--
-- @
-- validateISBN10 "0-345-81602-1" == Right (ISBN10 "0345816021")
-- validateISBN10 "0345816021"    == Right (ISBN10 "0345816021")
-- validateISBN10 "0-807-01429-X" == Right (ISBN10 "080701429X")
-- validateISBN10 "0-345-816"     == Left InvalidInputLength
-- validateISBN10 "X-345-81602-1" == Left IllegalCharactersInBody
-- validateISBN10 "0-345-81602-B" == Left IllegalCharacterAsCheckDigit
-- validateISBN10 "0-345-81602-3" == Left InvalidCheckDigit
-- @
validateISBN10 :: Text -> Either ISBN10ValidationError ISBN10
validateISBN10 input = do
    let inputWithoutHyphens = Text.filter (/= '-') input

    unless (Text.length inputWithoutHyphens == 10) $
        Left InvalidInputLength

    let invalidBodyCharacters = Text.filter (not . isNumericCharacter) (Text.init inputWithoutHyphens)

    unless (Text.length invalidBodyCharacters == 0) $
        Left IllegalCharactersInBody

    unless (isValidISBN10CheckDigit $ Text.last inputWithoutHyphens) $
        Left IllegalCharacterAsCheckDigit

    unless (confirmISBN10CheckDigit inputWithoutHyphens) $
        Left InvalidCheckDigit

    pure $ ISBN10 inputWithoutHyphens


-- | Possible validation errors resulting from ISBN-10 validation.
data ISBN10ValidationError
    = InvalidInputLength           -- ^ The length of the input string is not 10 characters, not counting hyphens
    | IllegalCharactersInBody      -- ^ The first nine characters of the ISBN-10 input contain non-numeric characters
    | IllegalCharacterAsCheckDigit -- ^ The check digit of the ISBN-10 is not a valid character (@0-9@ or @\'X\'@)
    | InvalidCheckDigit            -- ^ The check digit not valid for the given ISBN-10 string
    deriving (Show, Eq)


-- | Convert an 'ISBN10ValidationError' into a human-friendly error message.
renderISBN10ValidationError :: ISBN10ValidationError -> Text
renderISBN10ValidationError validationError =
    case validationError of
        InvalidInputLength ->
            "An ISBN-10 must be 10 characters, not counting hyphens"

        IllegalCharactersInBody ->
            "The first nine characters of an ISBN-10 must all be numbers"

        IllegalCharacterAsCheckDigit ->
            "The last character of the supplied ISBN-10 must be a number or the letter 'X'"

        InvalidCheckDigit ->
            "The supplied ISBN-10 is not valid"


-- | Confirms that the check digit of an ISBN-10 is correct. Assumes that the
-- input consists of 9 numeric characters followed by a legal check digit
-- character (@0-9@ or @X@).
--
-- /Examples:/
--
-- @
-- confirmISBN10CheckDigit "0345816021" == True
-- confirmISBN10CheckDigit "080701429X" == True
-- @
confirmISBN10CheckDigit :: Text -> Bool
confirmISBN10CheckDigit isbn10 =
    calculateISBN10CheckDigitValue (Text.init isbn10) == isbn10CharToNumericValue (Text.last isbn10)


-- | Calculates an ISBN-10 check digit value using the standard check digit
-- calculation formula. Assumes that the input is 9 numeric characters. The
-- check digit value value can be any number in the range 0 to 10, the last
-- of which is represented by the symbol \'X\' in an ISBN-10.
--
-- See: <https://en.wikipedia.org/wiki/International_Standard_Book_Number#ISBN-10_check_digits>
--
-- /Examples:/
--
-- @
-- calculateISBN10CheckDigitValue "034581602" == 1
-- calculateISBN10CheckDigitValue "080701429" == 10
-- @
calculateISBN10CheckDigitValue :: Text -> Int
calculateISBN10CheckDigitValue input =
    go 10 (unpack input) 0
      where
        go n charList acc =
            case charList of
              [] -> (11 - (acc `mod` 11)) `mod` 11
              c:clist -> go (n - 1) clist (acc + isbn10CharToNumericValue c * n)


-- | Converts an ISBN-10 character to a numeric value. Valid input characters
-- include @0-9@ as well as @X@.
isbn10CharToNumericValue :: Char -> Int
isbn10CharToNumericValue 'X' = 10
isbn10CharToNumericValue  c  = digitToInt c

-- | Converts a numeric value to an ISBN-10 character. Valid input values
-- are the numbers from 0 to 10.
numericValueToISBN10Char :: Int -> Char
numericValueToISBN10Char 10 = 'X'
numericValueToISBN10Char c  = Text.head $ pack $ show c


-- | Validates a character as a valid ISBN-10 check digit character.  ISBN-10
-- check digit characters include @0-9@ as well as the symbol @'X'@. The lowercase
-- letter \'x\' is not considered valid.
isValidISBN10CheckDigit :: Char -> Bool
isValidISBN10CheckDigit char = char `elem` ("1234567890X" :: String)

-- | Determines whether a character is numeric (e.g. in the range of @0-9@)
isNumericCharacter :: Char -> Bool
isNumericCharacter char = char `elem` ("1234567890" :: String)


-- | Allows for the generation of ISBN-10 values without any validation. This should
-- only be used in special cases. For example, there have been several instances of
-- books published with an invalid ISBN-10
unsafeToISBN10 :: Text -> ISBN10
unsafeToISBN10 = ISBN10
