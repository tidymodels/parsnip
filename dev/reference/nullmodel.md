# Fit a simple, non-informative model

Fit a single mean, set of quantiles, or largest class model.
`nullmodel()` is the underlying computational function for the
[`null_model()`](https://parsnip.tidymodels.org/dev/reference/null_model.md)
specification.

## Usage

``` r
nullmodel(x, ...)

# Default S3 method
nullmodel(x = NULL, y, quantile_levels = NULL, ...)

# S3 method for class 'nullmodel'
print(x, ...)

# S3 method for class 'nullmodel'
predict(object, new_data = NULL, type = NULL, ...)
```

## Arguments

- x:

  An optional matrix or data frame of predictors. These values are not
  used in the model fit.

- ...:

  Optional arguments (not yet used).

- y:

  A numeric vector (for regression) or factor (for classification) of
  outcomes.

- quantile_levels:

  A numeric vector of probabilities used to compute outcome quantiles.
  When `NULL`, the mean is computed for numeric outcomes.

- object:

  An object of class `nullmodel`.

- new_data:

  A matrix or data frame of predictors, only used to determine the
  number of predictions to return.

- type:

  Either `"raw"` (for regression), `"quantile"` (for quantile
  regression), `"class"`, or `"prob"` (for classification).

## Value

The output of `nullmodel()` is a list of class `nullmodel` with
elements:

- `call`: The function call.

- `value`: The mean or quantiles of `y`, or the most prevalent class.

- `levels`: When `y` is a factor, a vector of levels; `NULL` otherwise.

- `quantile_levels`: The probabilities used to compute quantiles, or
  `NULL`.

- `pct`: When `y` is a factor, a data frame with a column for each class
  (`NULL` otherwise). The column for the most prevalent class has the
  proportion of the training samples with that class; the other columns
  are zero.

- `n`: The number of elements in `y`.

`predict.nullmodel()` returns a factor, numeric vector, or matrix
depending on the class of `y` and whether quantiles were requested. All
predictions are always the same.

## Details

`nullmodel()` emulates other model building functions, but returns the
simplest model possible given a training set: a single mean or set of
quantiles for numeric outcomes and the most prevalent class for factor
outcomes. When class probabilities are requested, the percentage of the
training set samples with the most prevalent class is returned.

## Examples

``` r

outcome <- factor(sample(letters[1:2],
                         size = 100,
                         prob = c(.1, .9),
                         replace = TRUE))
useless <- nullmodel(y = outcome)
useless
#> Null Classification Model
#> Predicted Value: b 
predict(useless, matrix(NA, nrow = 5))
#> [1] b b b b b
#> Levels: a b
```
