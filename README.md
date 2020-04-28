# haskell-wav

## Overview

`haskell-wav` is a library that parses WAV files and extracts its metadata into an AST.
It allows developers to programmatically query the contents of these types of files in their Haskell programs.

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

## How to Develop

1. Install dependencies: `stack install`
1. Start the application: `stack build`

Until commit hooks are setup, before committing changes:

* Run `hpack` if changes are made to `package.yaml` or new modules are added.
* Run `hlint src/*` and `stylish-haskell -i src/*`
