# bnfu

A Clojure library designed to generate parsers from bnf grammars.

I don't know a lot about parsers, so the generated parsers are most
likely not any good.

The generated parsers take seqs as inputs.

The generated parsers are not perfect, certain formulations of
grammars can cause the parsers to blow the stack, etc.

## Usage

`[org.thelastcitadel/bnfu 0.0.1]` in your project.clj

```clojure
(bnf "foo/bar.bnf")
```

where foo/bar.bnf is a bnf as a file on the classpath well generate
functions for the parser (bnf is a macro), with a main entry point
named after the first rule in the grammar like parse-<rule-name>.

the parse tree is hiccup style.

## License

Copyright © 2013 Kevin Downey

Distributed under the Eclipse Public License, the same as Clojure.
