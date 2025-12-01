# Flexible discriminant analysis via earth

`mda::fda()` (in conjunction with
[`earth::earth()`](https://rdrr.io/pkg/earth/man/earth.html) can fit a
nonlinear discriminant analysis model that uses nonlinear features
created using multivariate adaptive regression splines (MARS). This
function can fit classification models.

## Details

For this engine, there is a single mode: classification

### Tuning Parameters

This model has 3 tuning parameter:

- `num_terms`: \# Model Terms (type: integer, default: (see below))

- `prod_degree`: Degree of Interaction (type: integer, default: 1L)

- `prune_method`: Pruning Method (type: character, default: ‘backward’)

The default value of `num_terms` depends on the number of columns (`p`):
`min(200, max(20, 2 * p)) + 1`. Note that `num_terms = 1` is an
intercept-only model.

### Translation from parsnip to the original package

The **discrim** extension package is required to fit this model.

    library(discrim)

    discrim_flexible(
      num_terms = integer(0),
      prod_degree = integer(0),
      prune_method = character(0)
    ) |>
      translate()

    ## Flexible Discriminant Model Specification (classification)
    ##
    ## Main Arguments:
    ##   num_terms = integer(0)
    ##   prod_degree = integer(0)
    ##   prune_method = character(0)
    ##
    ## Computational engine: earth
    ##
    ## Model fit template:
    ## mda::fda(formula = missing_arg(), data = missing_arg(), weights = missing_arg(),
    ##     nprune = integer(0), degree = integer(0), pmethod = character(0),
    ##     method = earth::earth)

### Preprocessing requirements

Factor/categorical predictors need to be converted to numeric values
(e.g., dummy or indicator variables) for this engine. When using the
formula method via
[`fit()`](https://parsnip.tidymodels.org/reference/fit.md), parsnip will
convert factor columns to indicators.

### Case weights

This model can utilize case weights during model fitting. To use them,
see the documentation in
[case_weights](https://parsnip.tidymodels.org/reference/case_weights.md)
and the examples on `tidymodels.org`.

The [`fit()`](https://generics.r-lib.org/reference/fit.html) and
[`fit_xy()`](https://generics.r-lib.org/reference/fit_xy.html) arguments
have arguments called `case_weights` that expect vectors of case
weights.

### Prediction types

    parsnip:::get_from_env("discrim_flexible_predict") |>
      dplyr::select(mode, type)

    ## # A tibble: 3 x 2
    ##   mode           type
    ##   <chr>          <chr>
    ## 1 classification class
    ## 2 classification prob
    ## 3 classification raw

### References

- Hastie, Tibshirani & Buja (1994) Flexible Discriminant Analysis by
  Optimal Scoring, *Journal of the American Statistical Association*,
  89:428, 1255-1270

- Friedman (1991). Multivariate Adaptive Regression Splines. *The Annals
  of Statistics*, 19(1), 1-67.
