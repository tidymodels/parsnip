


For this engine, there is a single mode: censored regression

## Tuning Parameters



This model has 2 tuning parameters:

- `penalty`: Amount of Regularization (type: double, default: see below)

- `mixture`: Proportion of Lasso Penalty (type: double, default: 1.0)

A value of `mixture = 1` corresponds to a pure lasso model, while `mixture = 0` indicates ridge regression.

The `penalty` parameter has no default and requires a single numeric value. For more details about this, and the `glmnet` model in general, see [parsnip::glmnet-details].

## Translation from parsnip to the original package

There is a parsnip extension package required to fit this model to this mode: **censored**.


```r
library(censored)

proportional_hazards(penalty = double(1), mixture = double(1)) %>% 
  set_engine("glmnet") %>% 
  translate()
```

```
## Proportional Hazards Model Specification (censored regression)
## 
## Main Arguments:
##   penalty = 0
##   mixture = double(1)
## 
## Computational engine: glmnet 
## 
## Model fit template:
## censored::glmnet_fit_wrapper(formula = missing_arg(), data = missing_arg(), 
##     family = missing_arg(), alpha = double(1))
```

## Preprocessing requirements


Factor/categorical predictors need to be converted to numeric values (e.g., dummy or indicator variables) for this engine. When using the formula method via \\code{\\link[=fit.model_spec]{fit.model_spec()}}, parsnip will convert factor columns to indicators.


Predictors should have the same scale. One way to achieve this is to center and 
scale each so that each predictor has mean zero and a variance of one.
By default, [glmnet::glmnet()] uses the argument `standardize = TRUE` to center and scale the data. 


## Other details

The model does not fit an intercept. 

The model formula (which is required) can include _special_ terms, such as [survival::strata()]. This allows the baseline hazard to differ between groups contained in the function. The column used inside `strata()` is treated as qualitative no matter its type. This is different than the syntax offered by the [glmnet::glmnet()] package (i.e., [glmnet::stratifySurv()]) which is not recommended here. 

For example, in this model, the numeric column `rx` is used to estimate two different baseline hazards for each value of the column:




```r
library(survival)
library(censored)
library(dplyr)
library(tidyr)

mod <- 
  proportional_hazards(penalty = 0.01) %>% 
  set_engine("glmnet", nlambda = 5) %>% 
  fit(Surv(futime, fustat) ~ age + ecog.ps + strata(rx), data = ovarian)

pred_data <- data.frame(age = c(50, 50), ecog.ps = c(1, 1), rx = c(1, 2))

# Different survival probabilities for different values of 'rx'
predict(mod, pred_data, type = "survival", time = 500) %>% 
  bind_cols(pred_data) %>% 
  unnest(.pred)
```

```
## # A tibble: 2 × 5
##   .time .pred_survival   age ecog.ps    rx
##   <dbl>          <dbl> <dbl>   <dbl> <dbl>
## 1   500          0.666    50       1     1
## 2   500          0.769    50       1     2
```

Note that columns used in the `strata()` function _will_ also be estimated in the regular portion of the model (i.e., within the linear predictor).

# Linear predictor values

Since risk regression and parametric survival models are modeling different characteristics (e.g. relative hazard versus event time), their linear predictors will be going in opposite directions. 

For example, for parametric models, the linear predictor _increases with time_. For proportional hazards models the linear predictor _decreases with time_ (since hazard is increasing). As such, the linear predictors for these two quantities will have opposite signs.

tidymodels does not treat different models differently when computing performance metrics.  To standardize across model types, the default for proportional hazards models is to have _increasing values with time_. As a result, the sign of the linear predictor will be the opposite of the value produced by the `predict()` method in the \pkg{survival} package. 

This behavior can be changed by using the `increasing` argument when calling `predict()` on a \pkg{parsnip} model object. 

# References

 - Simon N, Friedman J, Hastie T, Tibshirani R. 2011. "Regularization Paths for Cox’s Proportional Hazards Model via Coordinate Descent." _Journal of Statistical Software_, Articles 39 (5): 1–13. \doi{10.18637/jss.v039.i05}.
 
 - Hastie T, Tibshirani R, Wainwright M. 2015. _Statistical Learning with Sparsity_. CRC Press.
 
 - Kuhn M, Johnson K. 2013. _Applied Predictive Modeling_. Springer.

