module Data.ISBN.Types where

import           Data.Text


-- | Data type for representing ISBN values. Should be created safely using
-- 'Data.ISBN.validateISBN' (or either one of 'Data.ISBN.validateISBN10' or
-- 'Data.ISBN.validateISBN13'), but any string can be coerced into  an @ISBN@
-- value using 'Data.ISBN.unsafeToISBN10' or 'Data.ISBN.unsafeToISBN13'.
data ISBN
    = ISBN10 Text -- ^ An ISBN-10 value. Consists of 9 digits followed by a base-11 check digit (@0-9@ or @\'X\'@).
    | ISBN13 Text -- ^ An ISBN-13 value. Consists of 12 digits followed by a base-10 check digit (@0-9@).
    deriving (Show, Eq)

