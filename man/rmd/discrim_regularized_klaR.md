


For this engine, there is a single mode: classification

## Tuning Parameters




This model has 2 tuning parameter:

- `frac_common_cov`: Fraction of the Common Covariance Matrix (type: double, default: (see below))

- `frac_identity`: Fraction of the Identity Matrix (type: double, default: (see below))

Some special cases for the RDA model: 

* `frac_identity = 0` and `frac_common_cov = 1` is a linear discriminant analysis (LDA) model. 

* `frac_identity = 0` and `frac_common_cov = 0` is a quadratic discriminant analysis (QDA) model. 



## Translation from parsnip to the original package

The **discrim** extension package is required to fit this model.


``` r
library(discrim)

discrim_regularized(frac_identity = numeric(0), frac_common_cov = numeric(0)) |> 
  set_engine("klaR") |> 
  translate()
```

```
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
```

## Preprocessing requirements


Factor/categorical predictors need to be converted to numeric values (e.g., dummy or indicator variables) for this engine. When using the formula method via \\code{\\link[=fit.model_spec]{fit()}}, parsnip will convert factor columns to indicators.


Variance calculations are used in these computations within each outcome class. For this reason,  _zero-variance_ predictors (i.e., with a single unique value) within each class should be eliminated before fitting the model. 



## Case weights


The underlying model implementation does not allow for case weights. 

## References

 - Friedman, J (1989). Regularized Discriminant Analysis. _Journal of the American Statistical Association_, 84, 165-175.

 - Kuhn, M, and K Johnson. 2013. _Applied Predictive Modeling_. Springer.
