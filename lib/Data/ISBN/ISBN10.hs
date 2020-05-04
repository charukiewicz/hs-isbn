{-# LANGUAGE OverloadedStrings #-}

module Data.ISBN.ISBN10
    ( ISBN10
    , ISBN10ValidationError(..)
    , validateISBN10
    , confirmISBN10CheckDigit
    , calculateISBN10CheckDigit
    , isValidISBN10CheckDigit
    ) where

import           Control.Monad
import           Data.Char
import           Data.Text     as Text

newtype ISBN10 = ISBN10 Text
    deriving (Show, Eq)

{-| Possible validation errors resulting from ISBN 10 validation.
-}
data ISBN10ValidationError
    = InputTooLong
    | IllegalCharactersInBody
    | IllegalCharacterInCheckDigit
    | InvalidCheckDigit
    deriving (Show)

renderISBN10ValidationError :: ISBN10ValidationError -> Text
renderISBN10ValidationError validationError =
    case validationError of
        InputTooLong ->
            "Input must be 10 characters, not counting hyphens"

        IllegalCharactersInBody ->
            "The first nine characters of an ISBN 10 must all be numeric"

        IllegalCharacterInCheckDigit ->
            "The last character of the supplied ISBN 10 is not a valid check digit (0-9 or 'X')"

        InvalidCheckDigit ->
            "Check digit of the supplied ISBN 10 is not valid"


{-| Validation of ISBN 10 values
-}
validateISBN10 :: Text -- ^ The string to validate
               -> Either ISBN10ValidationError ISBN10
validateISBN10 input = do
    let inputWithoutHyphens = Text.filter (\c -> c /= '-') input

    when (Text.length inputWithoutHyphens /= 10) $
        Left InputTooLong

    let invalidBodyCharacters = Text.filter (not . isNumericCharacter) (Text.init inputWithoutHyphens)

    when (Text.length invalidBodyCharacters /= 0) $
        Left IllegalCharactersInBody

    when (not $ isValidISBN10CheckDigit $ Text.last inputWithoutHyphens) $
        Left IllegalCharacterInCheckDigit

    when (not $ confirmISBN10CheckDigit inputWithoutHyphens) $
        Left InvalidCheckDigit

    Right $ ISBN10 inputWithoutHyphens


{-| Confirms that the check digit of an ISBN 10 is correct. Assumes that the
input consists of 9 numeric characters followed by a legal check digit
character (@0-9@ or @X@).

/Example:/

@
confirmISBN10CheckDigit "0345816021" -- /True/
@
-}
confirmISBN10CheckDigit :: Text -> Bool
confirmISBN10CheckDigit isbn10 =
    (calculateISBN10CheckDigit $ Text.init isbn10) == (isbnCharToNumericValue $ Text.last isbn10)


{-|
   Calculates an ISBN 10 check digit using the standard check digit calculation
   formula. Assumes that the input is 9 numeric characters.

   See: https://en.wikipedia.org/wiki/International_Standard_Book_Number#ISBN-10_check_digits
-}
calculateISBN10CheckDigit :: Text -> Int
calculateISBN10CheckDigit input =
    go 10 (unpack input) 0
      where
        go n charList sum =
            case charList of
              [] -> (11 - (sum `mod` 11)) `mod` 11
              c:clist -> go (n - 1) clist (sum + (isbnCharToNumericValue c) * n)


{-| Converts an ISBN 10 character to a numeric value. Valid input characters
include @0-9@ as well as @X@.
-}
isbnCharToNumericValue :: Char -> Int
isbnCharToNumericValue 'X' = 10
isbnCharToNumericValue  c  = digitToInt c


{-| Validates a character as a valid ISBN 10 check digit character.  ISBN 10
check digit characters include @0-9@ as well as the symbol @X@. The lowercase
letter \'x\' is not considered valid.
-}
isValidISBN10CheckDigit :: Char -> Bool
isValidISBN10CheckDigit char = char `elem` ("1234567890X" :: String)

isNumericCharacter :: Char -> Bool
isNumericCharacter char = char `elem` ("1234567890" :: String)
