# quartzlang

Author: Radosław Waśko

The grammar is defined in separate file: `grammar.pdf`

## Language
Quartz is a functional language featuring:
- static type system with basic inference (although recursive types have to be specified as well as top-level types, so that each exported function is documented with a proper type signature)
- polymorphism
- lazy evaluation (for example `if` is defined as a function inside the language)
- custom polymorphic, recursive datatypes (`List a`, `Maybe a` are defined inside the language)
- simple pattern matching (without support for nesting)
- a builtin IO structure with do-notation that allow for imperative programming
- importing modules (for now all definitions are brought as-is, no `import qualified`)
- `fuel` - every function call is counted, so that it's possible to run programs with given `call-quota` and they are terminated if they exceed it, additionally there's a `limit` function that allows for further limitting the quota inside of the language itself;
  this might be useful for using the language as a script language and making sure some malicious scripts don't do too much computation in the background by setting some sane limits.

### Modules
In each file, all toplevel declarations need to have fully specified types - so that they serve as the documentation on the exported methods.

`import`ing a module searches for a `module.quartz` file in the same directory as the current file / current directory for REPL and then looks at `QUARTZ_MODULES_PATH` environment variable which should list paths delimited by a colon and searches these paths in the order they appear there; the first path that contains the proper file is loaded.

Right now there is no way to hide methods from being exported, but a preceding underscode may be used as a convention and in the future it may be the case that names starting with underscore will be excluded from export.

Each identifier may be qualified or not.
The qualified identifier just uses the symbol from the module it belongs to (the module has to have been imported somewhere in the same file though).
The not-qualified identifer first looks at the local scope (arguments of functions / lambdas we are in, local definitions and later definitions from current module with classic shadowing rules) and if the identifier is not found there, all modules that have been imported in the current file are searched for such a identifier; if exactly one of them matches, it is used, if multiple ones match a "ambiguous reference" error is raised telling the user to use the full name - thus the order of modules imported is not important as only identifiers appearing in a single module can be used without a qualifier.

### Examples
`example.quartz` is a program in the language showcasing most of its features.


## Ocenianie
Komentarz odnośnie oceniania:
- spodziewana liczba punktów: 30
- z wypisanych wymagań jedyne niespełnione to zagnieżdżany pattern-matching, liczę na to, że dodatkowe funkcjonalności takie jak:
  - imperatywność dzięki strukturze IO
  - limitowanie ilości wywołań funkcji za pomocą `limit`
  - wczytywanie modułów
  nadrabiają ten brak. Jeżeli nie, to postaram się zmienić plany.
