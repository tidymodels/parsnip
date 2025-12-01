# Bagged trees via C5.0

`baguette::bagger()` creates an collection of decision trees forming an
ensemble. All trees in the ensemble are combined to produce a final
prediction.

## Details

For this engine, there is a single mode: classification

### Tuning Parameters

This model has 1 tuning parameters:

- `min_n`: Minimal Node Size (type: integer, default: 2L)

### Translation from parsnip to the original package (classification)

The **baguette** extension package is required to fit this model.

    library(baguette)

    bag_tree(min_n = integer()) |>
      set_engine("C5.0") |>
      set_mode("classification") |>
      translate()

    ## Bagged Decision Tree Model Specification (classification)
    ##
    ## Main Arguments:
    ##   cost_complexity = 0
    ##   min_n = integer()
    ##
    ## Computational engine: C5.0
    ##
    ## Model fit template:
    ## baguette::bagger(x = missing_arg(), y = missing_arg(), weights = missing_arg(),
    ##     minCases = integer(), base_model = "C5.0")

### Preprocessing requirements

This engine does not require any special encoding of the predictors.
Categorical predictors can be partitioned into groups of factor levels
(e.g. `{a, c}` vs `{b, d}`) when splitting at a node. Dummy variables
are not required for this model.

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

    parsnip:::get_from_env("bag_mars_predict") |>
      dplyr::filter(engine == "C5.0") |>
      dplyr::select(mode, type)

    ## # A tibble: 0 x 2
    ## # i 2 variables: mode <chr>, type <chr>

### References

- Breiman, L. 1996. “Bagging predictors”. Machine Learning. 24 (2):
  123-140

- Kuhn, M, and K Johnson. 2013. *Applied Predictive Modeling*. Springer.
