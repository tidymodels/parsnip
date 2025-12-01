# Determine largest value of mtry from formula. This function potentially caps the value of `mtry` based on a formula and data set. This is a safe approach for survival and/or multivariate models.

Determine largest value of mtry from formula. This function potentially
caps the value of `mtry` based on a formula and data set. This is a safe
approach for survival and/or multivariate models.

## Usage

``` r
max_mtry_formula(mtry, formula, data)
```

## Arguments

- mtry:

  An initial value of `mtry` (which may be too large).

- formula:

  A model formula.

- data:

  The training set (data frame).

## Value

A value for `mtry`.

## Examples

``` r
# should be 9
max_mtry_formula(200, cbind(wt, mpg) ~ ., data = mtcars)
#> [1] 9
```
