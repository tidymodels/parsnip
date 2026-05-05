# Ordinal decision trees via CART

`rpartScore::rpartScore()` extends
[rpart](https://parsnip.tidymodels.org/dev/reference/details_decision_tree_rpart.md)
to fit classification trees for ordinal responses.

## Details

For this engine, there is a single mode: classification

### Tuning parameters

This model has 3 tuning parameters:

- `tree_depth`: Tree Depth (type: integer, default: 30L)

- `min_n`: Minimal Node Size (type: integer, default: 2L)

- `cost_complexity`: Cost-Complexity Parameter (type: double, default:
  0.01)

### Translation from parsnip to the original package

    decision_tree(
      tree_depth = integer(1),
      min_n = integer(1),
      cost_complexity = double(1)
    ) |>
      set_engine("rpartScore") |>
      set_mode("classification") |>
      translate()

    ## Decision Tree Model Specification (classification)
    ##
    ## Main Arguments:
    ##   cost_complexity = double(1)
    ##   tree_depth = integer(1)
    ##   min_n = integer(1)
    ##
    ## Computational engine: rpartScore
    ##
    ## Model fit template:
    ## ordered::rpartScore_wrapper(formula = missing_arg(), data = missing_arg(),
    ##     weights = missing_arg(), cp = double(1), maxdepth = integer(1),
    ##     minsplit = min_rows(0L, data))

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

    parsnip:::get_from_env("decision_tree_predict") |>
      dplyr::filter(engine == "rpartScore") |>
      dplyr::select(mode, type)

    ## # A tibble: 1 x 2
    ##   mode           type
    ##   <chr>          <chr>
    ## 1 classification class

### References

- Galimberti G, Soffritti G, Di Maso M. 2012. Classification Trees for
  Ordinal Responses in R: The rpartScore Package. *Journal of
  Statistical Software* 47(10):1-25. .

- Kuhn, M, and K Johnson. 2013. *Applied Predictive Modeling*. Springer.
