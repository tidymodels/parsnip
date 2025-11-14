# Knit engine-specific documentation

Knit engine-specific documentation

## Usage

``` r
knit_engine_docs(pattern = NULL)
```

## Arguments

- pattern:

  A regular expression to specify which files to knit. The default knits
  all engine documentation files.

## Value

A tibble with column `file` for the file name and `result` (a character
vector that echos the output file name or, when there is a failure, the
error message).

## Details

This function will check whether the known parsnip extension packages,
engine specific packages, and a few other ancillary packages are
installed. Users will be prompted to install anything required to create
the engine documentation.
