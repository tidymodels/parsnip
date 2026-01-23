# Random forests via partykit

[`partykit::cforest()`](https://rdrr.io/pkg/partykit/man/cforest.html)
fits a model that creates a large number of decision trees, each
independent of the others. The final prediction uses all predictions
from the individual trees and combines them.

## Details

For this engine, there are multiple modes: censored regression,
regression, and classification

### Tuning Parameters

This model has 3 tuning parameters:

- `trees`: \# Trees (type: integer, default: 500L)

- `min_n`: Minimal Node Size (type: integer, default: 20L)

- `mtry`: \# Randomly Selected Predictors (type: integer, default: 5L)

### Translation from parsnip to the original package (regression)

The **bonsai** extension package is required to fit this model.

    library(bonsai)

    rand_forest() |>
      set_engine("partykit") |>
      set_mode("regression") |>
      translate()

    ## Random Forest Model Specification (regression)
    ##
    ## Computational engine: partykit
    ##
    ## Model fit template:
    ## parsnip::cforest_train(formula = missing_arg(), data = missing_arg(),
    ##     weights = missing_arg())

### Translation from parsnip to the original package (classification)

The **bonsai** extension package is required to fit this model.

    library(bonsai)

    rand_forest() |>
      set_engine("partykit") |>
      set_mode("classification") |>
      translate()

    ## Random Forest Model Specification (classification)
    ##
    ## Computational engine: partykit
    ##
    ## Model fit template:
    ## parsnip::cforest_train(formula = missing_arg(), data = missing_arg(),
    ##     weights = missing_arg())

[`parsnip::cforest_train()`](https://parsnip.tidymodels.org/dev/reference/ctree_train.md)
is a wrapper around
[`partykit::cforest()`](https://rdrr.io/pkg/partykit/man/cforest.html)
(and other functions) that makes it easier to run this model.

## Translation from parsnip to the original package (censored regression)

The **censored** extension package is required to fit this model.

    library(censored)

    rand_forest() |>
      set_engine("partykit") |>
      set_mode("censored regression") |>
      translate()

    ## Random Forest Model Specification (censored regression)
    ##
    ## Computational engine: partykit
    ##
    ## Model fit template:
    ## parsnip::cforest_train(formula = missing_arg(), data = missing_arg(),
    ##     weights = missing_arg())

`censored::cond_inference_surv_cforest()` is a wrapper around
[`partykit::cforest()`](https://rdrr.io/pkg/partykit/man/cforest.html)
(and other functions) that makes it easier to run this model.

### Preprocessing requirements

This engine does not require any special encoding of the predictors.
Categorical predictors can be partitioned into groups of factor levels
(e.g. `{a, c}` vs `{b, d}`) when splitting at a node. Dummy variables
are not required for this model.

### Case weights

This model can utilize case weights during model fitting. To use them,
see the documentation in
[case_weights](https://parsnip.tidymodels.org/dev/reference/case_weights.md)
and the examples on `tidymodels.org`.

The [`fit()`](https://generics.r-lib.org/reference/fit.html) and
[`fit_xy()`](https://generics.r-lib.org/reference/fit_xy.html) arguments
have arguments called `case_weights` that expect vectors of case
weights.

### Prediction types

    parsnip:::get_from_env("rand_forest_predict") |>
      dplyr::filter(engine == "partykit") |>
      dplyr::select(mode, type)

    ## # A tibble: 5 x 2
    ##   mode                type
    ##   <chr>               <chr>
    ## 1 censored regression time
    ## 2 censored regression survival
    ## 3 regression          numeric
    ## 4 classification      class
    ## 5 classification      prob

### Other details

Predictions of type `"time"` are predictions of the median survival
time.

### References

- [partykit: A Modular Toolkit for Recursive Partytioning in
  R](https://jmlr.org/papers/v16/hothorn15a.html)

- Kuhn, M, and K Johnson. 2013. *Applied Predictive Modeling*. Springer.
