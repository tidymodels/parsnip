# Quadratic discriminant analysis via MASS

[`MASS::qda()`](https://rdrr.io/pkg/MASS/man/qda.html) fits a model that
estimates a multivariate distribution for the predictors separately for
the data in each class (Gaussian with separate covariance matrices).
Bayes' theorem is used to compute the probability of each class, given
the predictor values.

## Details

For this engine, there is a single mode: classification

### Tuning Parameters

This engine has no tuning parameters.

### Translation from parsnip to the original package

The **discrim** extension package is required to fit this model.

    library(discrim)

    discrim_quad() |>
      set_engine("MASS") |>
      translate()

    ## Quadratic Discriminant Model Specification (classification)
    ##
    ## Computational engine: MASS
    ##
    ## Model fit template:
    ## MASS::qda(formula = missing_arg(), data = missing_arg())

### Preprocessing requirements

Factor/categorical predictors need to be converted to numeric values
(e.g., dummy or indicator variables) for this engine. When using the
formula method via
[`fit()`](https://parsnip.tidymodels.org/dev/reference/fit.md), parsnip
will convert factor columns to indicators.

Variance calculations are used in these computations within each outcome
class. For this reason, *zero-variance* predictors (i.e., with a single
unique value) within each class should be eliminated before fitting the
model.

### Case weights

The underlying model implementation does not allow for case weights.

### References

- Kuhn, M, and K Johnson. 2013. *Applied Predictive Modeling*. Springer.
