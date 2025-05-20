


For this engine, there is a single mode: classification

## Tuning Parameters

This engine has no tuning parameter arguments in [discrim_linear()]. 

However, there are a few engine-specific parameters that can be set or optimized when calling [set_engine()]:

* `lambda`: the shrinkage parameters for the correlation matrix. This maps to the \pkg{dials} parameter [dials::shrinkage_correlation()].

* `lambda.var`: the shrinkage parameters for the predictor variances. This maps to [dials::shrinkage_variance()].

* `lambda.freqs`: the shrinkage parameters for the class frequencies. This maps to [dials::shrinkage_frequencies()].

* `diagonal`: a logical to make the model covariance diagonal or not. This maps to [dials::diagonal_covariance()].

## Translation from parsnip to the original package

The **discrim** extension package is required to fit this model.


``` r
library(discrim)

discrim_linear() |> 
  set_engine("sda") |> 
  translate()
```

```
## Linear Discriminant Model Specification (classification)
## 
## Computational engine: sda 
## 
## Model fit template:
## sda::sda(Xtrain = missing_arg(), L = missing_arg(), verbose = FALSE)
```

## Preprocessing requirements


Factor/categorical predictors need to be converted to numeric values (e.g., dummy or indicator variables) for this engine. When using the formula method via \\code{\\link[=fit.model_spec]{fit()}}, parsnip will convert factor columns to indicators.


Variance calculations are used in these computations so _zero-variance_ predictors (i.e., with a single unique value) should be eliminated before fitting the model. 



## Case weights


The underlying model implementation does not allow for case weights. 

## References

 - Ahdesmaki, A., and K. Strimmer. 2010. Feature selection in omics prediction problems using cat scores and false non-discovery rate control. Ann. Appl. Stat. 4: 503-519. [Preprint](https://arxiv.org/abs/0903.2003).
