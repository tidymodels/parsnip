# Locate and show errors/warnings in engine-specific documentation

Locate and show errors/warnings in engine-specific documentation

## Usage

``` r
list_md_problems()
```

## Value

A tibble with column `file` for the file name, `line` indicating the
line where the error/warning occurred, and `problem` showing the
error/warning message.
