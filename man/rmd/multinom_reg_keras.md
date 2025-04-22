


For this engine, there is a single mode: classification

## Tuning Parameters



This model has one tuning parameter:

- `penalty`: Amount of Regularization (type: double, default: 0.0)

For `penalty`, the amount of regularization is _only_ L2 penalty (i.e., ridge or weight decay). 

## Translation from parsnip to the original package


``` r
multinom_reg(penalty = double(1)) |> 
  set_engine("keras") |> 
  translate()
```

```
## Multinomial Regression Model Specification (classification)
## 
## Main Arguments:
##   penalty = double(1)
## 
## Computational engine: keras 
## 
## Model fit template:
## parsnip::keras_mlp(x = missing_arg(), y = missing_arg(), penalty = double(1), 
##     hidden_units = 1, act = "linear")
```

[keras_mlp()] is a parsnip wrapper around keras code for neural networks. This model fits a linear regression as a network with a single hidden unit. 

## Preprocessing requirements


Factor/categorical predictors need to be converted to numeric values (e.g., dummy or indicator variables) for this engine. When using the formula method via \\code{\\link[=fit.model_spec]{fit()}}, parsnip will convert factor columns to indicators.


Predictors should have the same scale. One way to achieve this is to center and 
scale each so that each predictor has mean zero and a variance of one.

## Case weights


The underlying model implementation does not allow for case weights. 

## Saving fitted model objects


Models fitted with this engine may require native serialization methods to be properly saved and/or passed between R sessions. To learn more about preparing fitted models for serialization, see the bundle package.

## Examples 

The "Fitting and Predicting with parsnip" article contains [examples](https://parsnip.tidymodels.org/articles/articles/Examples.html#multinom-reg-keras) for `multinom_reg()` with the `"keras"` engine.

## References

 - Hoerl, A., & Kennard, R. (2000). _Ridge Regression: Biased Estimation for Nonorthogonal Problems_. Technometrics, 42(1), 80-86. 

