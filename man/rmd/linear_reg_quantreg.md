


For this engine, there is a single mode: quantile regression

This model has the same structure as the model fit by `lm()`, but instead of optimizing the sum of squared errors, it optimizes "quantile loss" in order to produce better estimates of the predictive distribution. 

## Tuning Parameters

This engine has no tuning parameters. 

## Translation from parsnip to the original package

This model only works with the `"quantile regression"` model and requires users to specify which areas of the distribution to predict via the `quantile_levels` argument. For example: 


``` r
linear_reg() |> 
  set_engine("quantreg") |> 
  set_mode("quantile regression", quantile_levels = (1:3) / 4) |> 
  translate()
```

```
## Linear Regression Model Specification (quantile regression)
## 
## Computational engine: quantreg 
## 
## Model fit template:
## quantreg::rq(formula = missing_arg(), data = missing_arg(), weights = missing_arg(), 
##     tau = quantile_levels)
```

```
## Quantile levels: 0.25, 0.5, and 0.75.
```

## Output format

When multiple quantile levels are predicted, there are multiple predicted values for each row of new data. The `predict()` method for this mode produces a column named `.pred_quantile` that has a special class of `"quantile_pred"`, and it contains the predictions for each row. 

For example: 


``` r
library(modeldata)
rlang::check_installed("quantreg")

n <- nrow(Chicago)
Chicago <- Chicago |> select(ridership, Clark_Lake)

Chicago_train <- Chicago[1:(n - 7), ]
Chicago_test  <- Chicago[(n - 6):n, ]

qr_fit <- 
  linear_reg() |> 
  set_engine("quantreg") |> 
  set_mode("quantile regression", quantile_levels = (1:3) / 4) |> 
  fit(ridership ~ Clark_Lake, data = Chicago_train)
qr_fit
```

```
## parsnip model object
## 
## Call:
## quantreg::rq(formula = ridership ~ Clark_Lake, tau = quantile_levels, 
##     data = data)
## 
## Coefficients:
##              tau= 0.25 tau= 0.50 tau= 0.75
## (Intercept) -0.2064189 0.2051549 0.8112286
## Clark_Lake   0.9820582 0.9862306 0.9777820
## 
## Degrees of freedom: 5691 total; 5689 residual
```

``` r
qr_pred <- predict(qr_fit, Chicago_test)
qr_pred
```

```
## # A tibble: 7 x 1
##   .pred_quantile
##        <qtls(3)>
## 1         [21.1]
## 2         [21.4]
## 3         [21.7]
## 4         [21.4]
## 5         [19.5]
## 6         [6.88]
## # i 1 more row
```

We can unnest these values and/or convert them to a rectangular format:  


``` r
as_tibble(qr_pred$.pred_quantile)
```

```
## # A tibble: 21 x 3
##   .pred_quantile .quantile_levels  .row
##            <dbl>            <dbl> <int>
## 1           20.6             0.25     1
## 2           21.1             0.5      1
## 3           21.5             0.75     1
## 4           20.9             0.25     2
## 5           21.4             0.5      2
## 6           21.8             0.75     2
## # i 15 more rows
```

``` r
as.matrix(qr_pred$.pred_quantile)
```

```
##           [,1]      [,2]      [,3]
## [1,] 20.590627 21.090561 21.517717
## [2,] 20.863639 21.364733 21.789541
## [3,] 21.190665 21.693148 22.115142
## [4,] 20.879352 21.380513 21.805185
## [5,] 19.047814 19.541193 19.981622
## [6,]  6.435241  6.875033  7.423968
## [7,]  6.062058  6.500265  7.052411
```

## Preprocessing requirements


Factor/categorical predictors need to be converted to numeric values (e.g., dummy or indicator variables) for this engine. When using the formula method via \\code{\\link[=fit.model_spec]{fit()}}, parsnip will convert factor columns to indicators.

## Case weights


This model can utilize case weights during model fitting. To use them, see the documentation in [case_weights] and the examples on `tidymodels.org`. 

The `fit()` and `fit_xy()` arguments have arguments called `case_weights` that expect vectors of case weights. 

## Saving fitted model objects


This model object contains data that are not required to make predictions. When saving the model for the purpose of prediction, the size of the saved object might be substantially reduced by using functions from the [butcher](https://butcher.tidymodels.org) package.

## Examples 

The "Fitting and Predicting with parsnip" article contains [examples](https://parsnip.tidymodels.org/articles/articles/Examples.html#linear-reg-quantreg) for `linear_reg()` with the `"quantreg"` engine.

## References

 - Waldmann, E. (2018). Quantile regression: a short story on how and why. _Statistical Modelling_, 18(3-4), 203-218.
