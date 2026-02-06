# Multivariate adaptive regression splines (MARS) via earth

[`earth::earth()`](https://rdrr.io/pkg/earth/man/earth.html) fits a
generalized linear model that uses artificial features for some
predictors. These features resemble hinge functions and the result is a
model that is a segmented regression in small dimensions.

## Details

For this engine, there are multiple modes: classification and regression

### Tuning Parameters

This model has 3 tuning parameters:

- `num_terms`: \# Model Terms (type: integer, default: see below)

- `prod_degree`: Degree of Interaction (type: integer, default: 1L)

- `prune_method`: Pruning Method (type: character, default: ‘backward’)

Parsnip changes the default range for `num_terms` to `c(50, 500)`.

### Translation from parsnip to the original package (regression)

    mars(num_terms = integer(1), prod_degree = integer(1), prune_method = character(1)) |>
      set_engine("earth") |>
      set_mode("regression") |>
      translate()

    ## MARS Model Specification (regression)
    ##
    ## Main Arguments:
    ##   num_terms = integer(1)
    ##   prod_degree = integer(1)
    ##   prune_method = character(1)
    ##
    ## Computational engine: earth
    ##
    ## Model fit template:
    ## earth::earth(formula = missing_arg(), data = missing_arg(), weights = missing_arg(),
    ##     nprune = integer(1), degree = integer(1), pmethod = character(1),
    ##     keepxy = TRUE)

### Translation from parsnip to the original package (classification)

    mars(num_terms = integer(1), prod_degree = integer(1), prune_method = character(1)) |>
      set_engine("earth") |>
      set_mode("classification") |>
      translate()

    ## MARS Model Specification (classification)
    ##
    ## Main Arguments:
    ##   num_terms = integer(1)
    ##   prod_degree = integer(1)
    ##   prune_method = character(1)
    ##
    ## Engine-Specific Arguments:
    ##   glm = list(family = stats::binomial)
    ##
    ## Computational engine: earth
    ##
    ## Model fit template:
    ## earth::earth(formula = missing_arg(), data = missing_arg(), weights = missing_arg(),
    ##     nprune = integer(1), degree = integer(1), pmethod = character(1),
    ##     glm = list(family = stats::binomial), keepxy = TRUE)

An alternate method for using MARs for categorical outcomes can be found
in
[`discrim_flexible()`](https://parsnip.tidymodels.org/dev/reference/discrim_flexible.md).

### Preprocessing requirements

Factor/categorical predictors need to be converted to numeric values
(e.g., dummy or indicator variables) for this engine. When using the
formula method via
[`fit()`](https://parsnip.tidymodels.org/dev/reference/fit.md), parsnip
will convert factor columns to indicators.

### Case weights

This model can utilize case weights during model fitting. To use them,
see the documentation in
[case_weights](https://parsnip.tidymodels.org/dev/reference/case_weights.md)
and the examples on `tidymodels.org`.

The [`fit()`](https://generics.r-lib.org/reference/fit.html) and
[`fit_xy()`](https://generics.r-lib.org/reference/fit_xy.html) arguments
have arguments called `case_weights` that expect vectors of case
weights.

Note that the `earth` package documentation has: “In the current
implementation, *building models with weights can be slow*.”

### Prediction types

    parsnip:::get_from_env("mars_predict") |>
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

### Examples

The “Fitting and Predicting with parsnip”
[article](https://www.tidymodels.org/learn/models/parsnip-predictions/)
contains examples for
[`mars()`](https://parsnip.tidymodels.org/dev/reference/mars.md) with
the `"earth"` engine.

### References

- Friedman, J. 1991. “Multivariate Adaptive Regression Splines.” *The
  Annals of Statistics*, vol. 19, no. 1, pp. 1-67.

- Milborrow, S. [“Notes on the earth
  package.”](http://www.milbo.org/doc/earth-notes.pdf)

- Kuhn, M, and K Johnson. 2013. *Applied Predictive Modeling*. Springer.
