# TabICL via brulee

`brulee::brulee_tab_icl()` uses a pretrained tabular foundation model to
make predictions via in-context learning.

## Details

For this engine, there are multiple modes: classification and regression

### Tuning Parameters

This model has 2 tuning parameters:

- `num_estimators`: \# Estimators (type: integer, default: 8L)

- `softmax_temperature`: Softmax Temperature (type: double, default:
  0.9)

TabICL is a pretrained (prior-data fitted) network; no weights are
updated when the model is fit. The training set is ingested at fit time
and predictions are made via in-context learning.

Other engine arguments of interest:

- `normalization`: a character vector of per-member normalization
  methods (`"none"` or `"YeoJohnson"`).

- `training_set_limit`: the maximum number of training set rows used for
  in-context learning.

- `device`: the torch device to use (e.g., `"cpu"`).

### Translation from parsnip to the original package (regression)

    tabular_icl(
      num_estimators = integer(1),
      softmax_temperature = double(1)
    ) |>
      set_engine("brulee") |>
      set_mode("regression") |>
      translate()

    ## tabular icl Model Specification (regression)
    ##
    ## Main Arguments:
    ##   num_estimators = integer(1)
    ##   softmax_temperature = double(1)
    ##
    ## Computational engine: brulee
    ##
    ## Model fit template:
    ## brulee::brulee_tab_icl(formula = missing_arg(), data = missing_arg(),
    ##     num_estimators = integer(1), softmax_temperature = double(1))

### Translation from parsnip to the original package (classification)

    tabular_icl(
      num_estimators = integer(1),
      softmax_temperature = double(1)
    ) |>
      set_engine("brulee") |>
      set_mode("classification") |>
      translate()

    ## tabular icl Model Specification (classification)
    ##
    ## Main Arguments:
    ##   num_estimators = integer(1)
    ##   softmax_temperature = double(1)
    ##
    ## Computational engine: brulee
    ##
    ## Model fit template:
    ## brulee::brulee_tab_icl(formula = missing_arg(), data = missing_arg(),
    ##     num_estimators = integer(1), softmax_temperature = double(1))

### Preprocessing requirements

`brulee_tab_icl()` converts each predictor column to a numeric value
internally: factor and character columns are ordinal-encoded and numeric
columns are standardized (with an optional Yeo-Johnson transformation)
inside the model. There is *no need to pre-encode factors as
indicators*; a wide one-hot expansion degrades prediction quality for
this model. Predictors also do not need to be scaled by the user.

### Case weights

The underlying model implementation does not allow for case weights.

### Prediction types

    parsnip:::get_from_env("tabular_icl_predict") |>
      dplyr::filter(engine == "brulee") |>
      dplyr::select(mode, type)

    ## # A tibble: 3 x 2
    ##   mode           type
    ##   <chr>          <chr>
    ## 1 classification class
    ## 2 classification prob
    ## 3 regression     numeric

### References

- Qu, J., Holzmüller, D., Varoquaux, G., & Morvan, M. L. (2025). TabICL:
  A tabular foundation model for in-context learning on large data.
  *arXiv preprint* arXiv:2502.05564.

- Qu, J., Holzmüller, D., Varoquaux, G., & Morvan, M. L. (2026).
  TabICLv2: A better, faster, scalable, and open tabular foundation
  model. *arXiv preprint* arXiv:2602.11139.
