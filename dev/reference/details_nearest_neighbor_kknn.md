# K-nearest neighbors via kknn

[`kknn::train.kknn()`](https://rdrr.io/pkg/kknn/man/train.kknn.html)
fits a model that uses the `K` most similar data points from the
training set to predict new samples.

## Details

For this engine, there are multiple modes: classification and regression

### Tuning Parameters

This model has 3 tuning parameters:

- `neighbors`: \# Nearest Neighbors (type: integer, default: 5L)

- `weight_func`: Distance Weighting Function (type: character, default:
  ‘optimal’)

- `dist_power`: Minkowski Distance Order (type: double, default: 2.0)

Parsnip changes the default range for `neighbors` to `c(1, 15)` and
`dist_power` to `c(1/10, 2)`.

### Translation from parsnip to the original package (regression)

    nearest_neighbor(
      neighbors = integer(1),
      weight_func = character(1),
      dist_power = double(1)
    ) |>
      set_engine("kknn") |>
      set_mode("regression") |>
      translate()

    ## K-Nearest Neighbor Model Specification (regression)
    ##
    ## Main Arguments:
    ##   neighbors = integer(1)
    ##   weight_func = character(1)
    ##   dist_power = double(1)
    ##
    ## Computational engine: kknn
    ##
    ## Model fit template:
    ## kknn::train.kknn(formula = missing_arg(), data = missing_arg(),
    ##     ks = min_rows(0L, data, 5), kernel = character(1), distance = double(1))

[`min_rows()`](https://parsnip.tidymodels.org/dev/reference/min_cols.md)
will adjust the number of neighbors if the chosen value if it is not
consistent with the actual data dimensions.

### Translation from parsnip to the original package (classification)

    nearest_neighbor(
      neighbors = integer(1),
      weight_func = character(1),
      dist_power = double(1)
    ) |>
      set_engine("kknn") |>
      set_mode("classification") |>
      translate()

    ## K-Nearest Neighbor Model Specification (classification)
    ##
    ## Main Arguments:
    ##   neighbors = integer(1)
    ##   weight_func = character(1)
    ##   dist_power = double(1)
    ##
    ## Computational engine: kknn
    ##
    ## Model fit template:
    ## kknn::train.kknn(formula = missing_arg(), data = missing_arg(),
    ##     ks = min_rows(0L, data, 5), kernel = character(1), distance = double(1))

### Preprocessing requirements

Factor/categorical predictors need to be converted to numeric values
(e.g., dummy or indicator variables) for this engine. When using the
formula method via
[`fit()`](https://parsnip.tidymodels.org/dev/reference/fit.md), parsnip
will convert factor columns to indicators.

Predictors should have the same scale. One way to achieve this is to
center and scale each so that each predictor has mean zero and a
variance of one.

### Examples

The “Fitting and Predicting with parsnip”
[article](https://www.tidymodels.org/learn/models/parsnip-predictions/)
contains examples for
[`nearest_neighbor()`](https://parsnip.tidymodels.org/dev/reference/nearest_neighbor.md)
with the `"kknn"` engine.

### Case weights

The underlying model implementation does not allow for case weights.

### Prediction types

    parsnip:::get_from_env("nearest_neighbor_predict") |>
      dplyr::select(mode, type)

    ## # A tibble: 5 x 2
    ##   mode           type
    ##   <chr>          <chr>
    ## 1 regression     numeric
    ## 2 regression     raw
    ## 3 classification class
    ## 4 classification prob
    ## 5 classification raw

### Saving fitted model objects

This model object contains data that are not required to make
predictions. When saving the model for the purpose of prediction, the
size of the saved object might be substantially reduced by using
functions from the [butcher](https://butcher.tidymodels.org) package.

### References

- Hechenbichler K. and Schliep K.P. (2004) [Weighted k-Nearest-Neighbor
  Techniques and Ordinal
  Classification](https://epub.ub.uni-muenchen.de/1769/), Discussion
  Paper 399, SFB 386, Ludwig-Maximilians University Munich

- Kuhn, M, and K Johnson. 2013. *Applied Predictive Modeling*. Springer.
