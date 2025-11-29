# Linear regression via keras/tensorflow

This model uses regularized least squares to fit models with numeric
outcomes.

## Details

For this engine, there is a single mode: regression

### Tuning Parameters

This model has one tuning parameter:

- `penalty`: Amount of Regularization (type: double, default: 0.0)

For `penalty`, the amount of regularization is *only* L2 penalty (i.e.,
ridge or weight decay).

### Translation from parsnip to the original package

    linear_reg(penalty = double(1)) |>
      set_engine("keras") |>
      translate()

    ## Linear Regression Model Specification (regression)
    ##
    ## Main Arguments:
    ##   penalty = double(1)
    ##
    ## Computational engine: keras
    ##
    ## Model fit template:
    ## parsnip::keras_mlp(x = missing_arg(), y = missing_arg(), penalty = double(1),
    ##     hidden_units = 1, act = "linear")

[`keras_mlp()`](https://parsnip.tidymodels.org/dev/reference/keras_mlp.md)
is a parsnip wrapper around keras code for neural networks. This model
fits a linear regression as a network with a single hidden unit.

### Preprocessing requirements

Factor/categorical predictors need to be converted to numeric values
(e.g., dummy or indicator variables) for this engine. When using the
formula method via
[`fit()`](https://parsnip.tidymodels.org/dev/reference/fit.md), parsnip
will convert factor columns to indicators.

Predictors should have the same scale. One way to achieve this is to
center and scale each so that each predictor has mean zero and a
variance of one.

### Case weights

The underlying model implementation does not allow for case weights.

### Prediction types

    parsnip:::get_from_env("linear_reg_predict") |>
      dplyr::filter(engine == "keras") |>
      dplyr::select(mode, type)

    ## # A tibble: 1 x 2
    ##   mode       type
    ##   <chr>      <chr>
    ## 1 regression numeric

### Examples

The “Fitting and Predicting with parsnip” article contains
[examples](https://parsnip.tidymodels.org/articles/articles/Examples.html#linear-reg-keras)
for
[`linear_reg()`](https://parsnip.tidymodels.org/dev/reference/linear_reg.md)
with the `"keras"` engine.

### References

- Hoerl, A., & Kennard, R. (2000). *Ridge Regression: Biased Estimation
  for Nonorthogonal Problems*. Technometrics, 42(1), 80-86.
