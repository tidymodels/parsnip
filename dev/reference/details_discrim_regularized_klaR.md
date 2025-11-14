# Regularized discriminant analysis via klaR

`klaR::rda()` fits a a model that estimates a multivariate distribution
for the predictors separately for the data in each class. The structure
of the model can be LDA, QDA, or some amalgam of the two. Bayes' theorem
is used to compute the probability of each class, given the predictor
values.

## Details

For this engine, there is a single mode: classification

### Tuning Parameters

This model has 2 tuning parameter:

- `frac_common_cov`: Fraction of the Common Covariance Matrix (type:
  double, default: (see below))

- `frac_identity`: Fraction of the Identity Matrix (type: double,
  default: (see below))

Some special cases for the RDA model:

- `frac_identity = 0` and `frac_common_cov = 1` is a linear discriminant
  analysis (LDA) model.

- `frac_identity = 0` and `frac_common_cov = 0` is a quadratic
  discriminant analysis (QDA) model.

### Translation from parsnip to the original package

The **discrim** extension package is required to fit this model.

    library(discrim)

    discrim_regularized(frac_identity = numeric(0), frac_common_cov = numeric(0)) |>
      set_engine("klaR") |>
      translate()

    ## Regularized Discriminant Model Specification (classification)
    ##
    ## Main Arguments:
    ##   frac_common_cov = numeric(0)
    ##   frac_identity = numeric(0)
    ##
    ## Computational engine: klaR
    ##
    ## Model fit template:
    ## klaR::rda(formula = missing_arg(), data = missing_arg(), lambda = numeric(0),
    ##     gamma = numeric(0))

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

- Friedman, J (1989). Regularized Discriminant Analysis. *Journal of the
  American Statistical Association*, 84, 165-175.

- Kuhn, M, and K Johnson. 2013. *Applied Predictive Modeling*. Springer.
