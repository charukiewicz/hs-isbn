ISBN Validation & Tools (work in progress)
==========================================

All books published by major publishers since 1970 have an International Standard Book Number (ISBN) associated with them. Prior to 2007, all ISBNs issued were ten digit ISBN-10 format. Since 2007, new ISBNs have been issued in the thirteen digit ISBN-13 format. See the [ISBN Wikipedia article](https://en.wikipedia.org/wiki/International_Standard_Book_Number) for more information.

## Overview

This library provides tools for both validating and working with ISBNs. For general use, only importing the `Data.ISBN` module is required, as it reexports all functionality for validating and converting between ISBN-10 and ISBN-13 values. The specific implementations for validation are located in the `Data.ISBN.ISBN10` and `Data.ISBN.ISBN13` modules, respectively.
