# Linear discriminant analysis via James-Stein-type shrinkage estimation

`sda::sda()` can fit a linear discriminant analysis model that can fit
models between classical discriminant analysis and diagonal discriminant
analysis.

## Details

For this engine, there is a single mode: classification

### Tuning Parameters

This engine has no tuning parameter arguments in
[`discrim_linear()`](https://parsnip.tidymodels.org/reference/discrim_linear.md).

However, there are a few engine-specific parameters that can be set or
optimized when calling
[`set_engine()`](https://parsnip.tidymodels.org/reference/set_engine.md):

- `lambda`: the shrinkage parameters for the correlation matrix. This
  maps to the parameter
  [`dials::shrinkage_correlation()`](https://dials.tidymodels.org/reference/shrinkage_correlation.html).

- `lambda.var`: the shrinkage parameters for the predictor variances.
  This maps to
  [`dials::shrinkage_variance()`](https://dials.tidymodels.org/reference/shrinkage_correlation.html).

- `lambda.freqs`: the shrinkage parameters for the class frequencies.
  This maps to
  [`dials::shrinkage_frequencies()`](https://dials.tidymodels.org/reference/shrinkage_correlation.html).

- `diagonal`: a logical to make the model covariance diagonal or not.
  This maps to
  [`dials::diagonal_covariance()`](https://dials.tidymodels.org/reference/shrinkage_correlation.html).

### Translation from parsnip to the original package

The **discrim** extension package is required to fit this model.

    library(discrim)

    discrim_linear() |>
      set_engine("sda") |>
      translate()

    ## Linear Discriminant Model Specification (classification)
    ##
    ## Computational engine: sda
    ##
    ## Model fit template:
    ## sda::sda(Xtrain = missing_arg(), L = missing_arg(), verbose = FALSE)

### Preprocessing requirements

Factor/categorical predictors need to be converted to numeric values
(e.g., dummy or indicator variables) for this engine. When using the
formula method via
[`fit()`](https://parsnip.tidymodels.org/reference/fit.md), parsnip will
convert factor columns to indicators.

Variance calculations are used in these computations so *zero-variance*
predictors (i.e., with a single unique value) should be eliminated
before fitting the model.

### Case weights

The underlying model implementation does not allow for case weights.

### Prediction types

    parsnip:::get_from_env("discrim_linear_predict") |>
      dplyr::filter(engine == "sda") |>
      dplyr::select(mode, type)

    ## # A tibble: 3 x 2
    ##   mode           type
    ##   <chr>          <chr>
    ## 1 classification class
    ## 2 classification prob
    ## 3 classification raw

### References

- Ahdesmaki, A., and K. Strimmer. 2010. Feature selection in omics
  prediction problems using cat scores and false non-discovery rate
  control. Ann. Appl. Stat. 4: 503-519.
  [Preprint](https://arxiv.org/abs/0903.2003).
