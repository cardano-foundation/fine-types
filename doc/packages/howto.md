# How to use packages

This document gives example of how to use [FineType's packaging system][spec].

  [spec]: ./spec

🚧 FIXME: This is a possible design for now. Subject to change and implementation.

# Command overview

* `fine-types check` — checks that a package passes all static checks.

* Error messages from `fine-types` should follow the [GNU Coding Standards for formatting error messages][errors] — I couldn't find any other standard.

  [errors]: https://www.gnu.org/prep/standards/html_node/Errors.html

# Examples

## Check that imports are complete

Given the following two modules and package definition,

```
-- file: ./X.fine
module X where

A = ℕ;
B = A + Bytes?;

-- file: ./Y.fine
module Y where

import X(A,B,C);

D = A 𐄂 B 𐄂 C;

-- file: ./P.fine
package P where

module X;
module Y;
```

performing the static check on the package `P`

```
fine-types check -i P.fine
```

should complain

```
P.fine:4:-Y.fine:2: Module X does not export identifier C.
```

## Checking two modules for equality

Given two definitions of a module `Address`, both depending on a module with signature `Crypto`,

```
-- file: ./Crypto.fine
signature Crypto where

KeyHash;
ScriptHash;

-- file: ./Allegra/Address.fine
module Address where

import Crypto(KeyHash, ScriptHash);

[…]

-- file: ./Shelley/Address.fine
module Address where

import Crypto(KeyHash, ScriptHash);

[…]
```

the following package definition

```
-- file: ./Allegra/DiffAllegra.fine
package DiffAllegra where

module Crypto;
module Address;
module ShelleyAddress = Address from "../Shelley/Address.fine";

assert Address equal ShelleyAddress;
```

passes the static check if the assertion is true, i.e. if the two modules `Address` export the same list of identifiers which represent equal types.

In other words, running the command

```
fine-types check -i ./Allegra/DiffAllegra.fine
```

succeeds with no output and exit code `EXIT_SUCCESS` (= `0`).
