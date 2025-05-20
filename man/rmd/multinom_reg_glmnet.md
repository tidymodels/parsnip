


For this engine, there is a single mode: classification

## Tuning Parameters



This model has 2 tuning parameters:

- `penalty`: Amount of Regularization (type: double, default: see below)

- `mixture`: Proportion of Lasso Penalty (type: double, default: 1.0)

The `penalty` parameter has no default and requires a single numeric value. For more details about this, and the `glmnet` model in general, see [glmnet-details]. As for `mixture`:

* `mixture = 1` specifies a pure lasso model,
* `mixture = 0`  specifies a ridge regression model, and
* `0 < mixture < 1` specifies an elastic net model, interpolating lasso and ridge.

## Translation from parsnip to the original package


``` r
multinom_reg(penalty = double(1), mixture = double(1)) |> 
  set_engine("glmnet") |> 
  translate()
```

```
## Multinomial Regression Model Specification (classification)
## 
## Main Arguments:
##   penalty = 0
##   mixture = double(1)
## 
## Computational engine: glmnet 
## 
## Model fit template:
## glmnet::glmnet(x = missing_arg(), y = missing_arg(), weights = missing_arg(), 
##     alpha = double(1), family = "multinomial")
```

## Preprocessing requirements


Factor/categorical predictors need to be converted to numeric values (e.g., dummy or indicator variables) for this engine. When using the formula method via \\code{\\link[=fit.model_spec]{fit()}}, parsnip will convert factor columns to indicators.


Predictors should have the same scale. One way to achieve this is to center and 
scale each so that each predictor has mean zero and a variance of one.
By default, [glmnet::glmnet()] uses the argument `standardize = TRUE` to center and scale the data. 

## Examples 

The "Fitting and Predicting with parsnip" article contains [examples](https://parsnip.tidymodels.org/articles/articles/Examples.html#multinom-reg-glmnet) for `multinom_reg()` with the `"glmnet"` engine.

## Case weights


This model can utilize case weights during model fitting. To use them, see the documentation in [case_weights] and the examples on `tidymodels.org`. 

The `fit()` and `fit_xy()` arguments have arguments called `case_weights` that expect vectors of case weights. 

## Sparse Data


This model can utilize sparse data during model fitting and prediction. Both sparse matrices such as dgCMatrix from the `Matrix` package and sparse tibbles from the `sparsevctrs` package are supported. See [sparse_data] for more information.

## Saving fitted model objects


This model object contains data that are not required to make predictions. When saving the model for the purpose of prediction, the size of the saved object might be substantially reduced by using functions from the [butcher](https://butcher.tidymodels.org) package.

## References

 - Hastie, T, R Tibshirani, and M Wainwright. 2015. _Statistical Learning with Sparsity_. CRC Press.
 
 - Kuhn, M, and K Johnson. 2013. _Applied Predictive Modeling_. Springer.

