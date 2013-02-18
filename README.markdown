# Requirements

 * ghc
 * cabal
 * llvm
 * happy
 * Summit (https://github.com/RobotGymnast/Summit)

# Installing

If you have cabal and llvm installed, you should be able to install Playmidi by calling

    scripts/install.sh

This will install the library and docs, as well as run the tests.

# Changing code

## Setting up

You can set up the build environment by running

    scripts/setup.sh

Doing so is **mandatory** before committing any code, as this also sets up pre-commit hooks.
All scripts should be run from the project root directory.

## Building

Builds can be run with

    scripts/build.sh

## Documentation

Haddock documentation can be generated using

    scripts/docgen.sh

By default, the documentation is generated to `dist/docs/html/`

## Tests

To run the tests after a successful build, run

    scripts/test.sh

## Code Standards

Coding is a language. You are expressing ideas, so they should be as clear, concise, and elegant as possible.

 * Wrap to 120 characters
 * Functions ending with a single quote usually require a transformation function as one of their parameters
 * When indenting multi-lined bodies, align SOMETHING visually (e.g. operators)
   or just use a multiple of four spaces (at least 8)
 * Indent a `where` keyword by 4 spaces, and the declarations within it by 8
 * If a `where` clause has more than one line in it, the `where` keyword should be on a distinct line from any code
 * Do not have more than one embedded subscope (A `let` inside a `where` is acceptable, but to be used sparingly)
