# Fuzzy conversions

These are substitutes for
[`as.matrix()`](https://rdrr.io/r/base/matrix.html) and
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) that
leave a sparse matrix as-is.

## Usage

``` r
maybe_matrix(x)

maybe_data_frame(x)
```

## Arguments

- x:

  A data frame, matrix, or sparse matrix.

## Value

A data frame, matrix, or sparse matrix.
