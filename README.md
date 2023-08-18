# FineTypes

🚧 WORK IN PROGRESS 🚧

## Overview

FineTypes is an interface description language (IDL) focussing on types. You can use it to specify data types that are interoperable between different programming languages.

Example:

```
TxOut = Addr × Value × (Datum ⊎ DataHash)? × Script?;
```

FineTypes allows you to

* Define data types concisely using mathematical notation.
* Map between data types using algebraic transformations, such as `(A + B) × C = (A × C) + (B × C)`.
* Export type definitions to different programming languages, currently:
    * Haskell
* Export type definitions to different data schemas, currently
    * OpenAPI: Schema Objects

FineTypes was originally conceived to be able to specify all types in the [Cardano ledger specification][cardano-ledger] in a way that is both machine readable and visually matches the PDF document.

## Contents

This repository contains

* The `fine-types` Haskell package and executable.
* The `cardano-ledger-types` package containing a reference specification of the types from the [Cardano ledger specification][cardano-ledger] in different eras (Shelley, …, Babbage).

  [cardano-ledger]: https://github.com/input-output-hk/cardano-ledger/releases/latest/

