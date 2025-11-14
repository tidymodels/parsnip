# C5.0 rule-based classification models

[`C50::C5.0()`](https://topepo.github.io/C5.0/reference/C5.0.html) fits
a model that derives feature rules from a tree for prediction. A single
tree or boosted ensemble can be used. `rules::c5_fit()` is a wrapper
around this function.

## Details

For this engine, there is a single mode: classification

### Tuning Parameters

This model has 2 tuning parameters:

- `trees`: \# Trees (type: integer, default: 1L)

- `min_n`: Minimal Node Size (type: integer, default: 2L)

Note that C5.0 has a tool for *early stopping* during boosting where
less iterations of boosting are performed than the number requested.
[`C5_rules()`](https://parsnip.tidymodels.org/dev/reference/C5_rules.md)
turns this feature off (although it can be re-enabled using
[`C50::C5.0Control()`](https://topepo.github.io/C5.0/reference/C5.0Control.html)).

### Translation from parsnip to the underlying model call (classification)

The **rules** extension package is required to fit this model.

    library(rules)

    C5_rules(
      trees = integer(1),
      min_n = integer(1)
    ) |>
      set_engine("C5.0") |>
      set_mode("classification") |>
      translate()

    ## C5.0 Model Specification (classification)
    ##
    ## Main Arguments:
    ##   trees = integer(1)
    ##   min_n = integer(1)
    ##
    ## Computational engine: C5.0
    ##
    ## Model fit template:
    ## rules::c5_fit(x = missing_arg(), y = missing_arg(), weights = missing_arg(),
    ##     trials = integer(1), minCases = integer(1))

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

### Saving fitted model objects

This model object contains data that are not required to make
predictions. When saving the model for the purpose of prediction, the
size of the saved object might be substantially reduced by using
functions from the [butcher](https://butcher.tidymodels.org) package.

### References

- Quinlan R (1992). “Learning with Continuous Classes.” Proceedings of
  the 5th Australian Joint Conference On Artificial Intelligence,
  pp. 343-348.

- Quinlan R (1993).”Combining Instance-Based and Model-Based Learning.”
  Proceedings of the Tenth International Conference on Machine Learning,
  pp. 236-243.

- Kuhn M and Johnson K (2013). *Applied Predictive Modeling*. Springer.
