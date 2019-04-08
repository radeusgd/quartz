# quartzlang

Author: Radosław Waśko

The grammar is defined in separate file: `grammar.pdf`

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

`example.quartz` is a program in the language showcasing most of its features.

Komentarz odnośnie oceniania:
- spodziewana liczba punktów: 30
- z wypisanych wymagań jedyne niespełnione to zagnieżdżany pattern-matching, liczę na to, że dodatkowe funkcjonalności takie jak:
  - imperatywność dzięki strukturze IO
  - limitowanie ilości wywołań funkcji za pomocą `limit`
  - wczytywanie modułów
  nadrabiają ten brak. Jeżeli nie, to postaram się zmienić plany.
