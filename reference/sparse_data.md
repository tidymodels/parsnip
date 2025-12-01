# Using sparse data with parsnip

You can figure out whether a given model engine supports sparse data by
calling `get_encoding("name of model")` and looking at the
`allow_sparse_x` column.

## Details

Using sparse data for model fitting and prediction shouldn't require any
additional configurations. Just pass in a sparse matrix such as
dgCMatrix from the `Matrix` package or a sparse tibble from the
sparsevctrs package to the data argument of
[`fit()`](https://generics.r-lib.org/reference/fit.html),
[`fit_xy()`](https://generics.r-lib.org/reference/fit_xy.html), and
[`predict()`](https://rdrr.io/r/stats/predict.html).

Models that don't support sparse data will try to convert to non-sparse
data with warnings. If conversion isn’t possible, an informative error
will be thrown.
