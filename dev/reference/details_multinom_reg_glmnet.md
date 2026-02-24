# Multinomial regression via glmnet

`glmnet::glmnet()` fits a model that uses linear predictors to predict
multiclass data using the multinomial distribution.

## Details

For this engine, there is a single mode: classification

### Tuning Parameters

This model has 2 tuning parameters:

- `penalty`: Amount of Regularization (type: double, default: see below)

- `mixture`: Proportion of Lasso Penalty (type: double, default: 1.0)

The `penalty` parameter has no default and requires a single numeric
value. For more details about this, and the `glmnet` model in general,
see
[glmnet-details](https://parsnip.tidymodels.org/dev/reference/glmnet-details.md).
As for `mixture`:

- `mixture = 1` specifies a pure lasso model,

- `mixture = 0` specifies a ridge regression model, and

- `0 < mixture < 1` specifies an elastic net model, interpolating lasso
  and ridge.

### Translation from parsnip to the original package

    multinom_reg(penalty = double(1), mixture = double(1)) |>
      set_engine("glmnet") |>
      translate()

    ## Multinomial Regression Model Specification (classification)
    ##
    ## Main Arguments:
    ##   penalty = 0
    ##   mixture = double(1)
    ##
    ## Computational engine: glmnet
    ##
    ## Model fit template:
    ## glmnet::glmnet(x = missing_arg(), y = missing_arg(), weights = missing_arg(),
    ##     alpha = double(1), family = "multinomial")

### Preprocessing requirements

Factor/categorical predictors need to be converted to numeric values
(e.g., dummy or indicator variables) for this engine. When using the
formula method via
[`fit()`](https://parsnip.tidymodels.org/dev/reference/fit.md), parsnip
will convert factor columns to indicators.

Predictors should have the same scale. One way to achieve this is to
center and scale each so that each predictor has mean zero and a
variance of one. By default, `glmnet::glmnet()` uses the argument
`standardize = TRUE` to center and scale the data.

### Examples

The “Fitting and Predicting with parsnip”
[article](https://www.tidymodels.org/learn/models/parsnip-predictions/)
contains examples for
[`multinom_reg()`](https://parsnip.tidymodels.org/dev/reference/multinom_reg.md)
with the `"glmnet"` engine.

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

    parsnip:::get_from_env("multinom_reg_predict") |>
      dplyr::filter(engine == "glmnet") |>
      dplyr::select(mode, type)

    ## # A tibble: 3 x 2
    ##   mode           type
    ##   <chr>          <chr>
    ## 1 classification class
    ## 2 classification prob
    ## 3 classification raw

### Sparse Data

This model can utilize sparse data during model fitting and prediction.
Both sparse matrices such as dgCMatrix from the `Matrix` package and
sparse tibbles from the `sparsevctrs` package are supported. See
[sparse_data](https://parsnip.tidymodels.org/dev/reference/sparse_data.md)
for more information.

### Saving fitted model objects

This model object contains data that are not required to make
predictions. When saving the model for the purpose of prediction, the
size of the saved object might be substantially reduced by using
functions from the [butcher](https://butcher.tidymodels.org) package.

### References

- Hastie, T, R Tibshirani, and M Wainwright. 2015. *Statistical Learning
  with Sparsity*. CRC Press.

- Kuhn, M, and K Johnson. 2013. *Applied Predictive Modeling*. Springer.
