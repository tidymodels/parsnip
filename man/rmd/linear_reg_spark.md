


For this engine, there is a single mode: regression

## Tuning Parameters



This model has 2 tuning parameters:

- `penalty`: Amount of Regularization (type: double, default: 0.0)

- `mixture`: Proportion of Lasso Penalty (type: double, default: 0.0)

For `penalty`, the amount of regularization includes both the L1 penalty (i.e., lasso) and the L2 penalty (i.e., ridge or weight decay). 

A value of `mixture = 1` corresponds to a pure lasso model, while `mixture = 0` indicates ridge regression.

## Translation from parsnip to the original package


```r
linear_reg(penalty = double(1), mixture = double(1)) %>% 
  set_engine("spark") %>% 
  translate()
```

```
## Linear Regression Model Specification (regression)
## 
## Main Arguments:
##   penalty = double(1)
##   mixture = double(1)
## 
## Computational engine: spark 
## 
## Model fit template:
## sparklyr::ml_linear_regression(x = missing_arg(), formula = missing_arg(), 
##     weight_col = missing_arg(), reg_param = double(1), elastic_net_param = double(1))
```

## Preprocessing requirements


Factor/categorical predictors need to be converted to numeric values (e.g., dummy or indicator variables) for this engine. When using the formula method via \\code{\\link[=fit.model_spec]{fit()}}, parsnip will convert factor columns to indicators.


Predictors should have the same scale. One way to achieve this is to center and 
scale each so that each predictor has mean zero and a variance of one.
By default, `ml_linear_regression()` uses the argument `standardization = TRUE` to center and scale the data. 

## Other details


For models created using the `"spark"` engine, there are several things to consider. 

* Only the formula interface to via `fit()` is available; using `fit_xy()` will generate an error. 
* The predictions will always be in a Spark table format. The names will be the same as documented but without the dots. 
* There is no equivalent to factor columns in Spark tables so class predictions are returned as character columns. 
* To retain the model object for a new R session (via `save()`), the `model$fit` element of the parsnip object should be serialized via `ml_save(object$fit)` and separately saved to disk. In a new session, the object can be reloaded and reattached to the parsnip object.

## References

 - Luraschi, J, K Kuo, and E Ruiz. 2019. _Mastering Spark with R_. O'Reilly Media
 
 - Hastie, T, R Tibshirani, and M Wainwright. 2015. _Statistical Learning with Sparsity_. CRC Press.
 
 - Kuhn, M, and K Johnson. 2013. _Applied Predictive Modeling_. Springer.

