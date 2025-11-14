# Linear discriminant analysis via flexible discriminant analysis

`mda::fda()` (in conjunction with `mda::gen.ridge()` can fit a linear
discriminant analysis model that penalizes the predictor coefficients
with a quadratic penalty (i.e., a ridge or weight decay approach).

## Details

For this engine, there is a single mode: classification

### Tuning Parameters

This model has 1 tuning parameter:

- `penalty`: Amount of Regularization (type: double, default: 1.0)

### Translation from parsnip to the original package

The **discrim** extension package is required to fit this model.

    library(discrim)

    discrim_linear(penalty = numeric(0)) |>
      set_engine("mda") |>
      translate()

    ## Linear Discriminant Model Specification (classification)
    ##
    ## Main Arguments:
    ##   penalty = numeric(0)
    ##
    ## Computational engine: mda
    ##
    ## Model fit template:
    ## mda::fda(formula = missing_arg(), data = missing_arg(), weights = missing_arg(),
    ##     lambda = numeric(0), method = mda::gen.ridge, keep.fitted = FALSE)

### Preprocessing requirements

Factor/categorical predictors need to be converted to numeric values
(e.g., dummy or indicator variables) for this engine. When using the
formula method via
[`fit()`](https://parsnip.tidymodels.org/dev/reference/fit.md), parsnip
will convert factor columns to indicators.

Variance calculations are used in these computations so *zero-variance*
predictors (i.e., with a single unique value) should be eliminated
before fitting the model.

### Case weights

This model can utilize case weights during model fitting. To use them,
see the documentation in
[case_weights](https://parsnip.tidymodels.org/dev/reference/case_weights.md)
and the examples on `tidymodels.org`.

The [`fit()`](https://generics.r-lib.org/reference/fit.html) and
[`fit_xy()`](https://generics.r-lib.org/reference/fit_xy.html) arguments
have arguments called `case_weights` that expect vectors of case
weights.

### References

- Hastie, Tibshirani & Buja (1994) Flexible Discriminant Analysis by
  Optimal Scoring, *Journal of the American Statistical Association*,
  89:428, 1255-1270
