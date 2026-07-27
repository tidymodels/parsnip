# Random survival forests via randomForestSRC

`randomForestSRC::rfsrc()` fits a random survival forest: a large number
of survival trees, each grown on a bootstrap sample of the data. The
final prediction averages the predictions from the individual trees.

## Details

For this engine, there is a single mode: censored regression

### Tuning Parameters

This model has 3 tuning parameters:

- `trees`: \# Trees (type: integer, default: 500L)

- `min_n`: Minimal Node Size (type: integer, default: 15L)

- `mtry`: \# Randomly Selected Predictors (type: integer, default:
  ceiling(sqrt(n_predictors)))

### Translation from parsnip to the original package (censored regression)

The **censored** extension package is required to fit this model.

    library(censored)

    rand_forest() |>
      set_engine("randomForestSRC") |>
      set_mode("censored regression") |>
      translate()

    ## Random Forest Model Specification (censored regression)
    ##
    ## Computational engine: randomForestSRC
    ##
    ## Model fit template:
    ## censored::rfsrc_train(formula = missing_arg(), data = missing_arg(),
    ##     weights = missing_arg())

`censored::rfsrc_train()` is a wrapper around `randomForestSRC::rfsrc()`
that makes it easier to run this model.

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
[`fit_xy()`](https://generics.r-lib.org/reference/fit_xy.html) functions
have arguments called `case_weights` that expect vectors of case
weights.

### Prediction types

    parsnip:::get_from_env("rand_forest_predict") |>
      dplyr::filter(engine == "randomForestSRC") |>
      dplyr::select(mode, type) |>
      print(n = Inf)

    ## # A tibble: 2 x 2
    ##   mode                type
    ##   <chr>               <chr>
    ## 1 censored regression time
    ## 2 censored regression survival

### Other details

Predictions of type `"time"` are predictions of the median survival
time.

### References

- Ishwaran H, Kogalur UB, Blackstone EH, Lauer MS. Random survival
  forests. Annals of Applied Statistics 2008; 2(3):841-860. .
