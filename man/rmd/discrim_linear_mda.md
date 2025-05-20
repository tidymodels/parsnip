


For this engine, there is a single mode: classification

## Tuning Parameters




This model has 1 tuning parameter:

- `penalty`: Amount of Regularization (type: double, default: 1.0)

## Translation from parsnip to the original package

The **discrim** extension package is required to fit this model.


``` r
library(discrim)

discrim_linear(penalty = numeric(0)) |> 
  set_engine("mda") |> 
  translate()
```

```
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
```

## Preprocessing requirements


Factor/categorical predictors need to be converted to numeric values (e.g., dummy or indicator variables) for this engine. When using the formula method via \\code{\\link[=fit.model_spec]{fit()}}, parsnip will convert factor columns to indicators.


Variance calculations are used in these computations so _zero-variance_ predictors (i.e., with a single unique value) should be eliminated before fitting the model. 



## Case weights


This model can utilize case weights during model fitting. To use them, see the documentation in [case_weights] and the examples on `tidymodels.org`. 

The `fit()` and `fit_xy()` arguments have arguments called `case_weights` that expect vectors of case weights. 

## References

 - Hastie, Tibshirani & Buja (1994) Flexible Discriminant Analysis by Optimal 
   Scoring, _Journal of the American Statistical Association_, 89:428, 1255-1270
