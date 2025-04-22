


For this engine, there is a single mode: classification

## Tuning Parameters



This model has 2 tuning parameters:

- `penalty`: Amount of Regularization (type: double, default: 0.0)

- `mixture`: Proportion of Lasso Penalty (type: double, default: 0.0)

For `penalty`, the amount of regularization includes both the L1 penalty (i.e., lasso) and the L2 penalty (i.e., ridge or weight decay). As for `mixture`:

* `mixture = 1` specifies a pure lasso model,
* `mixture = 0`  specifies a ridge regression model, and
* `0 < mixture < 1` specifies an elastic net model, interpolating lasso and ridge.

## Translation from parsnip to the original package


``` r
logistic_reg(penalty = double(1), mixture = double(1)) |> 
  set_engine("spark") |> 
  translate()
```

```
## Logistic Regression Model Specification (classification)
## 
## Main Arguments:
##   penalty = double(1)
##   mixture = double(1)
## 
## Computational engine: spark 
## 
## Model fit template:
## sparklyr::ml_logistic_regression(x = missing_arg(), formula = missing_arg(), 
##     weights = missing_arg(), reg_param = double(1), elastic_net_param = double(1), 
##     family = "binomial")
```

## Preprocessing requirements


Factor/categorical predictors need to be converted to numeric values (e.g., dummy or indicator variables) for this engine. When using the formula method via \\code{\\link[=fit.model_spec]{fit()}}, parsnip will convert factor columns to indicators.


Predictors should have the same scale. One way to achieve this is to center and 
scale each so that each predictor has mean zero and a variance of one.

By default, `ml_logistic_regression()` uses the argument `standardization = TRUE` to center and scale the data. 

## Case weights


This model can utilize case weights during model fitting. To use them, see the documentation in [case_weights] and the examples on `tidymodels.org`. 

The `fit()` and `fit_xy()` arguments have arguments called `case_weights` that expect vectors of case weights. 

Note that, for spark engines, the `case_weight` argument value should be a character string to specify the column with the numeric case weights. 

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

