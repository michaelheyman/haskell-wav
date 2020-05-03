# haskell-wav

## Overview

`haskell-wav` is a library that parses WAV files and extracts its metadata into an AST.
It allows developers to programmatically query the contents of these types of files in their Haskell programs.

## How to Develop

### Running the Application

1. Install dependencies: `stack install`
1. Start the application: `stack build`

### Continuous Compilation with GHCID

1. Install `ghcid`: `stack install ghcid`
2. Run `ghcid` while developing to compile and reload on code changes:

```bash
ghcid --command="stack ghci src/Parser.hs test/Spec.hs"
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