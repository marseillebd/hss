# HSS

The plan for HSS is to be a library that exposes useful tools for scripting,
and a small utility that allows you to execute Haskell scripts based on that library.

## The Interpreter

Run `hss <script>.hss` or use a shebang in your executable script.
The interpreter simply compiles your `<script>.hss` with the hss library (and no other dependencies),
with a number of extensions enabled, and `Prelude` replaced by `Hss`.
When that's done, it moves the completed executable to `.<script>` and executes it with the remaining arguments.
TODO: If `.<script>` already exists and is up-to-date, it will just run the precompiled binary.

You will need `cabal-install` version 3.0+ and a (quite) recent `ghc` (version 9.12+ at a guess).

## The Library

The hss library is really an alternative Prelude.
It's meant only to use common dependencies (bytestring, text, containers, base),
but to take an opinionated stand on which data types are to be preferred.

### Strings

We address the "Haskell has 5 string types??" meme:
- Use strict `Text` for textual data.
- Use strict `ByteString` for binary data.
- Use `OsPath` to interact with the OS for file access.
- Use streaming or ropes to deal with large string data.
- Avoid standard `String` as much as possible.

## Contributing

This project is very experimental right now.
I wouldn't recommend anyone contribute besides myself.

- [ ] explore how to organize Hss library
  - [x] take exports from Prelude
  - [ ] take exports from Data.{Functor,Applicative}, Control.Monad
  - [ ] take exports from Data.{ByteString,Text}(,.IO}
  - [ ] normalize/typeclass string functions
  - [ ] take exports from Data.{Maybe,Either,These}
  - [ ] packages: witherable, default
  - [ ] an alternative to Show/Read; a pretty-printer
- [ ] explore process creation, piping, capture, and redirection APIs (starting with Shh)
- [x] pin the hss library used by an hss interpreter

This readme is a functioning maskfile (and is also symlinked to `maskfile.md`).
It defines useful commands for developing this project.
Use `mask --help` in the project directory to see available commands, or read on.

## hack

> DELME: a basic smoke test on my own machine

```sh
if ! which hss; then echo >&2 'make sure `hss` is on the `PATH`'; exit 1; fi
cabal build && ./example.hss foo goo fish
```

### hack bootstrap

> DELME: a basic smoke test on my own machine

```sh
if ! which hss; then echo >&2 'make sure `hss` is on the `PATH`'; exit 1; fi
cabal build && hss app/Main.hs && app/.Main ./example.hss foo goo fish
```
## deps

> Show dependency tree with graphmod+graphviz.xdot

```sh
find src/ -name '*.hs' | xargs graphmod -q | xdot -
```
