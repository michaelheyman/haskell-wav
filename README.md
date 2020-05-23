# haskell-wav

## Overview

`haskell-wav` is a library that parses WAV files and extracts its metadata into an AST.
It allows developers to programmatically query the contents of these types of files in their Haskell programs.

## How to Develop

### Building the Application

1. Install dependencies: `stack install`
2. Start the application: `stack build`

### Running the Application

A `Main` has been provided to test the application. To test the application:

1. Run the application interactively with a REPL: `stack ghci`
2. Load the `Main` source file: `:l app/Main.hs`
3. Run the `main` program inside the REPL: `main`
4. Follow the prompts and input an absolute or relative path to a valid example WAV file:
    * `example/wav/about_time.wav`
    * `example/wav/cat_meow_x.wav`
    * `example/wav/CantinaBand3.wav`
    * `example/wav/StarWars3.wav`

### Continuous Compilation with GHCID

The following examples require `ghcid`. Install it via `stack install ghcid`.

Run `ghcid` while developing to compile and reload on code changes. Here are a few options

* Continuously compile application

    ```bash
    ghcid --command="stack ghci haskell-wav"
    ```

* Continuously compile application and tests

    ```bash
    ghcid --command="stack ghci haskell-wav:haskell-wav-test"
    ```

* Continuously compile application and run tests

    ```bash
    ghcid --command="stack ghci haskell-wav" --test=":!stack test"
    ```

### Before Committing Changes

1. Ensure that `pre-commit` and its configuration are installed
    * Install `pre-commit`: `brew install pre-commit` or `pip install pre-commit`
    * Install the commit hooks: `pre-commit install`
2. Run `hpack` if changes are made to `package.yaml` or new modules are added.

## WAV Specification

More information can be found in the [WAV Specification](https://en.wikipedia.org/wiki/WAV#Specification) page.

The WAV binary file format is split three chunks:

* `RIFF`
* `fmt`
* `data`

### `RIFF` chunk

| Field Name | Field Size (Bytes) | File Offset (Bytes) | Endian |
|------------|--------------------|---------------------|--------|
| ChunkID    |                  4 |                   0 | Big    |
| ChunkSize  |                  4 |                   4 | Little |
| Format     |                  4 |                   8 | Big    |

### `fmt` chunk

| Field Name    | Field Size (Bytes) | File Offset (Bytes) | Endian |
|---------------|--------------------|---------------------|--------|
| SubchunkID    |                  4 |                  12 | Big    |
| SubchunkSize  |                  4 |                  16 | Little |
| AudioFormat   |                  2 |                  20 | Little |
| NumChannels   |                  2 |                  22 | Little |
| SampleRate    |                  4 |                  24 | Little |
| ByteRate      |                  4 |                  28 | Little |
| BlockAlign    |                  2 |                  32 | Little |
| BitsPerSample |                  2 |                  34 | Little |

### `data` chunk

| Field Name   | Field Size (Bytes) | File Offset (Bytes) | Endian |
|--------------|--------------------|---------------------|--------|
| SubchunkID   |                  4 |                  36 | Big    |
| SubchunkSize |                  4 |                  40 | Little |
| Data         |       SubchunkSize |                  44 | Little |

## Documentation

This application is documented with haddock-compatible annotations.

To view the documentation:

* Build the documentation artifacts: `stack ghci --only-locals`
* View the generated docs in the browser: `stack haddock --open haskell-wav`
