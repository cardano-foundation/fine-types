# Packages in FineTypes

In larger projects, it is helpful to organize type definition in separate files. FineTypes' packaging system allows you to do that.

FineType files come in three different varieties:

1. module
2. signature
3. package

*Modules* contain the actual type definitions. *Signatures* are brief lists of types that are yet to be defined, and *packages* allow you to specify how multiple modules and signatures are combined.

In the simplest case, your project consists of a single *module* file. For more complicated projects, consider using *packages* and maybe even *signatures*.

The packaging system for FineTypes is a stripped-down dialect of [Backpack'14][backpack14], a packaging system conceived for the Haskell programming language. (See [Backpack in GHC][backpack17] for the current status in Haskell.)

  [backpack14]: https://plv.mpi-sws.org/backpack/
  [backpack17]: https://gitlab.haskell.org/ghc/ghc/-/wikis/backpack

## Module

*Modules* are the most important parts of your project, they contain the actual type definitions.

A module can `import` types from other modules or signatures. Consider the following example:

```
module ModuleX where

import ModuleY (Q);

A = Bytes;
B = Int;
C = (A × B?) + Q;
```

Here, the module `ModuleX` imports the name `Q` from `ModuleY`. By itself, `ModuleX` does not contain enough information to specify the type, say, `C` completely; we have to combine it with the actual definition of `Q` in `ModuleY`. A *package* describes where to find and how to combine modules.

Instead of a matching the `import` with a module `ModuleY`, we can also combine it with a *signature* `ModuleY`. This leaves the definition of the type `Q` intentionally open.

## Signatures

🚧 FIXME: Signatures to be implemented

A *signature* file contains a list of types to be defined.

Example:

```
signature ModuleY where

Q;
P;
```

Not much to see here. Signatures can be useful when

* exporting to other languages when some types are specified externally, and when
* performing static checks on some modules without including their dependencies in the check.

## Package

A *package* is essentially a list of modules. Importantly, modules must be listed in an order in which they can import each other: Each module must be listed after the modules that it imports. Instead of a module, a signature is also acceptable.

Example:

```
package Foo where

module ModuleY; -- must be listed *before* ModuleX
module ModuleX; -- import statements are resolved using the definitions above
```

One package can `include` another package. This means that all of its modules are brought into scope.

🚧 FIXME: To be reconsidered:
By convention, writing `module X` will refer to the file `./X.fine`. Module names containing by dots, such as `Very.Long.Module.Name`, will refer to files where the dots are replaced by path separators, that is to the file `./Very/Long/Module/Name.fine`. This convention can be overwritten.

🚧 FIXME: To be implemented:
If you want to bring only selected modules into scope, use an explicit list with parentheses. While doing so, you can rename with `as`.

```
package Bar where

include Foo (ModuleX, ModuleY as Y);
```

