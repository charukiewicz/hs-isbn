module Data.ISBN.Types where

import           Data.Text


-- | Data type for representing ISBN values. Should be created safely using
-- 'validateISBN10' or 'validateISBN13', but any string can be coerced into
-- an @ISBN@ value using 'unsafeToISBN10' or 'unsafeToISBN13'.
data ISBN
    = ISBN10 Text
    | ISBN13 Text
    deriving (Show, Eq)

