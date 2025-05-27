


For this engine, there are multiple modes: classification and regression

## Tuning Parameters



This model has 2 tuning parameters:

- `cost`: Cost (type: double, default: 1.0)

- `margin`: Insensitivity Margin (type: double, default: no default)

This engine fits models that are L2-regularized for L2-loss. In the [LiblineaR::LiblineaR()] documentation, these are types 1 (classification) and 11 (regression).

Parsnip changes the default range for `cost` to `c(-10, 5)`.

## Translation from parsnip to the original package (regression)


``` r
svm_linear(
  cost = double(1),
  margin = double(1)
) |>  
  set_engine("LiblineaR") |> 
  set_mode("regression") |> 
  translate()
```

```
## Linear Support Vector Machine Model Specification (regression)
## 
## Main Arguments:
##   cost = double(1)
##   margin = double(1)
## 
## Computational engine: LiblineaR 
## 
## Model fit template:
## LiblineaR::LiblineaR(x = missing_arg(), y = missing_arg(), C = double(1), 
##     svr_eps = double(1), type = 11)
```

## Translation from parsnip to the original package (classification)


``` r
svm_linear(
  cost = double(1)
) |> 
  set_engine("LiblineaR") |> 
  set_mode("classification") |> 
  translate()
```

```
## Linear Support Vector Machine Model Specification (classification)
## 
## Main Arguments:
##   cost = double(1)
## 
## Computational engine: LiblineaR 
## 
## Model fit template:
## LiblineaR::LiblineaR(x = missing_arg(), y = missing_arg(), C = double(1), 
##     type = 1)
```

The `margin` parameter does not apply to classification models. 

Note that the `LiblineaR` engine does not produce class probabilities. When optimizing the model using the tune package, the default metrics require class probabilities. To use the  `tune_*()` functions, a metric set must be passed as an argument that only contains metrics for hard class predictions (e.g., accuracy).

## Preprocessing requirements


Factor/categorical predictors need to be converted to numeric values (e.g., dummy or indicator variables) for this engine. When using the formula method via \\code{\\link[=fit.model_spec]{fit()}}, parsnip will convert factor columns to indicators.


Predictors should have the same scale. One way to achieve this is to center and 
scale each so that each predictor has mean zero and a variance of one.

## Case weights


The underlying model implementation does not allow for case weights. 

## Sparse Data


This model can utilize sparse data during model fitting and prediction. Both sparse matrices such as dgCMatrix from the `Matrix` package and sparse tibbles from the `sparsevctrs` package are supported. See [sparse_data] for more information.

## Examples 

The "Fitting and Predicting with parsnip" article contains [examples](https://parsnip.tidymodels.org/articles/articles/Examples.html#svm-linear-LiblineaR) for `svm_linear()` with the `"LiblineaR"` engine.

## References

 - Kuhn, M, and K Johnson. 2013. _Applied Predictive Modeling_. Springer.

