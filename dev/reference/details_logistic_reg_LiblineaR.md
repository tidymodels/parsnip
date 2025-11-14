# Logistic regression via LiblineaR

[`LiblineaR::LiblineaR()`](https://rdrr.io/pkg/LiblineaR/man/LiblineaR.html)
fits a generalized linear model for binary outcomes. A linear
combination of the predictors is used to model the log odds of an event.

## Details

For this engine, there is a single mode: classification

### Tuning Parameters

This model has 2 tuning parameters:

- `penalty`: Amount of Regularization (type: double, default: see below)

- `mixture`: Proportion of Lasso Penalty (type: double, default: 0)

For `LiblineaR` models, the value for `mixture` can either be 0 (for
ridge) or 1 (for lasso) but not other intermediate values. In the
[`LiblineaR::LiblineaR()`](https://rdrr.io/pkg/LiblineaR/man/LiblineaR.html)
documentation, these correspond to types 0 (L2-regularized) and 6
(L1-regularized).

Be aware that the `LiblineaR` engine regularizes the intercept. Other
regularized regression models do not, which will result in different
parameter estimates.

### Translation from parsnip to the original package

    logistic_reg(penalty = double(1), mixture = double(1)) |>
      set_engine("LiblineaR") |>
      translate()

    ## Logistic Regression Model Specification (classification)
    ##
    ## Main Arguments:
    ##   penalty = double(1)
    ##   mixture = double(1)
    ##
    ## Computational engine: LiblineaR
    ##
    ## Model fit template:
    ## LiblineaR::LiblineaR(x = missing_arg(), y = missing_arg(), cost = Inf,
    ##     type = double(1), verbose = FALSE)

### Preprocessing requirements

Factor/categorical predictors need to be converted to numeric values
(e.g., dummy or indicator variables) for this engine. When using the
formula method via
[`fit()`](https://parsnip.tidymodels.org/dev/reference/fit.md), parsnip
will convert factor columns to indicators.

Predictors should have the same scale. One way to achieve this is to
center and scale each so that each predictor has mean zero and a
variance of one.

### Sparse Data

This model can utilize sparse data during model fitting and prediction.
Both sparse matrices such as dgCMatrix from the `Matrix` package and
sparse tibbles from the `sparsevctrs` package are supported. See
[sparse_data](https://parsnip.tidymodels.org/dev/reference/sparse_data.md)
for more information.

### Examples

The “Fitting and Predicting with parsnip” article contains
[examples](https://parsnip.tidymodels.org/articles/articles/Examples.html#logistic-reg-LiblineaR)
for
[`logistic_reg()`](https://parsnip.tidymodels.org/dev/reference/logistic_reg.md)
with the `"LiblineaR"` engine.

### References

- Hastie, T, R Tibshirani, and M Wainwright. 2015. *Statistical Learning
  with Sparsity*. CRC Press.

- Kuhn, M, and K Johnson. 2013. *Applied Predictive Modeling*. Springer.
