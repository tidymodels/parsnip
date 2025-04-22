


For this engine, there is a single mode: classification

## Tuning Parameters



This model has 1 tuning parameters:

- `penalty`: Amount of Regularization (type: double, default: 0.0)

For `penalty`, the amount of regularization includes only the L2 penalty (i.e., ridge or weight decay). 

## Translation from parsnip to the original package


``` r
multinom_reg(penalty = double(1)) |> 
  set_engine("nnet") |> 
  translate()
```

```
## Multinomial Regression Model Specification (classification)
## 
## Main Arguments:
##   penalty = double(1)
## 
## Computational engine: nnet 
## 
## Model fit template:
## nnet::multinom(formula = missing_arg(), data = missing_arg(), 
##     decay = double(1), trace = FALSE)
```

## Preprocessing requirements


Factor/categorical predictors need to be converted to numeric values (e.g., dummy or indicator variables) for this engine. When using the formula method via \\code{\\link[=fit.model_spec]{fit()}}, parsnip will convert factor columns to indicators.


Predictors should have the same scale. One way to achieve this is to center and 
scale each so that each predictor has mean zero and a variance of one.

## Examples 

The "Fitting and Predicting with parsnip" article contains [examples](https://parsnip.tidymodels.org/articles/articles/Examples.html#multinom-reg-nnet) for `multinom_reg()` with the `"nnet"` engine.

## Case weights


The underlying model implementation does not allow for case weights. 

## Saving fitted model objects


This model object contains data that are not required to make predictions. When saving the model for the purpose of prediction, the size of the saved object might be substantially reduced by using functions from the [butcher](https://butcher.tidymodels.org) package.

## References

 - Luraschi, J, K Kuo, and E Ruiz. 2019. _Mastering nnet with R_. O'Reilly Media
 
 - Hastie, T, R Tibshirani, and M Wainwright. 2015. _Statistical Learning with Sparsity_. CRC Press.
 
 - Kuhn, M, and K Johnson. 2013. _Applied Predictive Modeling_. Springer.

