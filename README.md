# SQL-Solvent. SQL manipulation tool

<img src="https://github.com/nixorn/SQL-Solvent/blob/master/logo.png?raw=true" align="left" hspace="10" vspace="6">

**SQL-Solvent** is a Haskell library.

# Installation

Firstly, you need the Haskell Platform (https://www.haskell.org/platform/). Install it. Then say in shell:
 
```
cabal update
```
If you on Win, type this also:
```
cabal install cabal-install
```
That will upgrade your platform.
If you on *nix - do upgrading as you know.

Secondly, clone repository, cd into and say:
```
cabal sandbox init
cabal install
cabal build
```

run ./dist/build/solvent(.exe), go 127.0.0.1:8000

static files in ./static . Copy ./static in directory, where you run solvent(.exe)