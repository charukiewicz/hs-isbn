# Revision history for `isbn`

## 1.1.0.1 - July 30, 2020

- Changed implementation of `validateISBN` to make a copy of the input text, preventing space leaks
- Added some test cases for ISBN-13s with a '979' prefix

## 1.1.0.0 - July 28, 2020

- Changed exports in `Data.ISBN` module to not export the constructors of the `ISBN` data type
  - If constructors are needed, the `Data.Types.ISBN` module can be imported instead
- Added `isISBN10` and `isISBN13` functions to allow for checking of the opaque `ISBN` data type without the use of its value constructors

## 1.0.0.0 - May 17, 2020

- Initial release. Features:
  - General ISBN validation based off of text input, as well as specific ISBN-10 or ISBN-13 validation
  - Validation errors with human-friendly renderers, that can be used to display error messages to end users
  - ISBN-10 to ISBN-13 conversion and vise versa
