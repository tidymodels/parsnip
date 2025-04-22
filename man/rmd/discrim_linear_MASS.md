


For this engine, there is a single mode: classification

## Tuning Parameters

This engine has no tuning parameters. 

## Translation from parsnip to the original package

The **discrim** extension package is required to fit this model.


``` r
library(discrim)

discrim_linear() |> 
  set_engine("MASS") |> 
  translate()
```

```
## Linear Discriminant Model Specification (classification)
## 
## Computational engine: MASS 
## 
## Model fit template:
## MASS::lda(formula = missing_arg(), data = missing_arg())
```

## Preprocessing requirements


Factor/categorical predictors need to be converted to numeric values (e.g., dummy or indicator variables) for this engine. When using the formula method via \\code{\\link[=fit.model_spec]{fit()}}, parsnip will convert factor columns to indicators.


Variance calculations are used in these computations so _zero-variance_ predictors (i.e., with a single unique value) should be eliminated before fitting the model. 



## Case weights


The underlying model implementation does not allow for case weights. 

## References

 - Kuhn, M, and K Johnson. 2013. _Applied Predictive Modeling_. Springer.
