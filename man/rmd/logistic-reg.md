# Engine Details




Engines may have pre-set default arguments when executing the model fit call. 
For this type of model, the template of the fit calls are below.

## glm


```r
logistic_reg() %>% 
  set_engine("glm") %>% 
  translate()
```

```
## Logistic Regression Model Specification (classification)
## 
## Computational engine: glm 
## 
## Model fit template:
## stats::glm(formula = missing_arg(), data = missing_arg(), weights = missing_arg(), 
##     family = stats::binomial)
```

## glmnet


```r
logistic_reg(penalty = 0.1) %>% 
  set_engine("glmnet") %>% 
  translate()
```

```
## Logistic Regression Model Specification (classification)
## 
## Main Arguments:
##   penalty = 0.1
## 
## Computational engine: glmnet 
## 
## Model fit template:
## glmnet::glmnet(x = missing_arg(), y = missing_arg(), weights = missing_arg(), 
##     family = "binomial")
```

The glmnet engine requires a single value for the `penalty` argument (a number
or `tune()`), but the full regularization path is always fit
regardless of the value given to `penalty`. To pass in a custom sequence of
values for glmnet's `lambda`, use the argument `path_values` in `set_engine()`. 
This will assign the value of the glmnet `lambda` parameter without disturbing
the value given of `logistic_reg(penalty)`. For example: 


```r
logistic_reg(penalty = .1) %>% 
  set_engine("glmnet", path_values = c(0, 10^seq(-10, 1, length.out = 20))) %>% 
  translate()
```

```
## Logistic Regression Model Specification (classification)
## 
## Main Arguments:
##   penalty = 0.1
## 
## Computational engine: glmnet 
## 
## Model fit template:
## glmnet::glmnet(x = missing_arg(), y = missing_arg(), weights = missing_arg(), 
##     lambda = c(0, 10^seq(-10, 1, length.out = 20)), family = "binomial")
```

When fitting a pure ridge regression model (i.e., `penalty = 0`), we _strongly
suggest_ that you pass in a vector for `path_values` that includes zero. See 
[issue #431](https://github.com/tidymodels/parsnip/issues/431) for a discussion. 

When using `predict()`, the single `penalty` value used for prediction is the 
one specified in `logistic_reg()`. 

To predict on multiple penalties, use the `multi_predict()` function. 
This function returns a tibble with a list column called `.pred` containing 
all of the penalty results.


## LiblineaR


```r
logistic_reg() %>% 
  set_engine("LiblineaR") %>% 
  translate()
```

```
## Logistic Regression Model Specification (classification)
## 
## Computational engine: LiblineaR 
## 
## Model fit template:
## LiblineaR::LiblineaR(x = missing_arg(), y = missing_arg(), wi = missing_arg(), 
##     verbose = FALSE)
```

For `LiblineaR` models, the value for `mixture` can either be 0 (for ridge) or 1 
(for lasso) but not other intermediate values. In the `LiblineaR` documentation, 
these correspond to types 0 (L2-regularized) and 6 (L1-regularized).

Be aware that the `LiblineaR` engine regularizes the intercept. Other 
regularized regression models do not, which will result in different parameter estimates.

## stan


```r
logistic_reg() %>% 
  set_engine("stan") %>% 
  translate()
```

```
## Logistic Regression Model Specification (classification)
## 
## Computational engine: stan 
## 
## Model fit template:
## rstanarm::stan_glm(formula = missing_arg(), data = missing_arg(), 
##     weights = missing_arg(), family = stats::binomial, refresh = 0)
```

Note that the `refresh` default prevents logging of the estimation process.
Change this value in `set_engine()` to show the logs.

For prediction, the `stan` engine can compute posterior  intervals analogous to
confidence and prediction intervals. In  these instances, the units are the
original outcome and when  `std_error = TRUE`, the standard deviation of the
posterior  distribution (or posterior predictive distribution as  appropriate) is
returned.

## spark


```r
logistic_reg() %>% 
  set_engine("spark") %>% 
  translate()
```

```
## Logistic Regression Model Specification (classification)
## 
## Computational engine: spark 
## 
## Model fit template:
## sparklyr::ml_logistic_regression(x = missing_arg(), formula = missing_arg(), 
##     weight_col = missing_arg(), family = "binomial")
```

## keras


```r
logistic_reg() %>% 
  set_engine("keras") %>% 
  translate()
```

```
## Logistic Regression Model Specification (classification)
## 
## Computational engine: keras 
## 
## Model fit template:
## parsnip::keras_mlp(x = missing_arg(), y = missing_arg(), hidden_units = 1, 
##     act = "linear")
```


## Parameter translations

The standardized parameter names in parsnip can be mapped to their original 
names in each engine that has main parameters. Each engine typically has a 
different default value (shown in parentheses) for each parameter.


|**parsnip** |**glmnet** |**LiblineaR** |**spark**             |**keras**   |**brulee** |
|:-----------|:----------|:-------------|:---------------------|:-----------|:----------|
|penalty     |lambda     |cost          |reg_param (0)         |penalty (0) |penalty    |
|mixture     |alpha (1)  |type (0)      |elastic_net_param (0) |NA          |mixture    |
|epochs      |NA         |NA            |NA                    |NA          |epochs     |
|learn_rate  |NA         |NA            |NA                    |NA          |learn_rate |
|momentum    |NA         |NA            |NA                    |NA          |momentum   |
|stop_iter   |NA         |NA            |NA                    |NA          |stop_iter  |
