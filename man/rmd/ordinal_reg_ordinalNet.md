


For this engine, there is a single mode: classification

## Tuning Parameters



This model has 4 tuning parameters:

- `penalty`: Amount of Regularization (type: double, default: see below)

- `mixture`: Proportion of Lasso Penalty (type: double, default: 1.0)

- `ordinal_link`: Ordinal Link (type: character, default: logit)

- `odds_link`: Odds Link (type: character, default: cumulative)

## Translation from parsnip to the original package

The **ordered** extension package is required to fit this model.


``` r
library(ordered)

ordinal_reg(
  penalty = double(0),
  mixture = double(0),
  ordinal_link = character(0),
  odds_link = character(0)
) %>%
  set_engine("ordinalNet") %>%
  set_mode("classification") %>%
  translate()
```

```
## Ordinal Regression Model Specification (classification)
## 
## Main Arguments:
##   ordinal_link = character(0)
##   odds_link = character(0)
##   penalty = numeric(0)
##   mixture = double(0)
## 
## Computational engine: ordinalNet 
## 
## Model fit template:
## ordered::ordinalNet_wrapper(x = missing_arg(), y = missing_arg(), 
##     weights = missing_arg(), link = character(0), family = character(0), 
##     alpha = double(0), nLambda = 120L, lambdaMinRatio = 1e-08, 
##     includeLambda0 = TRUE)
```

## Controlling penalty values

`ordinalNet()`, like `glmnet()`, simultaneously computes a set of parameter estimates for multiple penalty values. Predictions can be made at these penalty values at the same time. However, unlike `glmnet()`, `ordinalNet()` does not interpolate if you want to predict a penalty values that is not exactly what it precomputed. Similarly, it cannot predict for models outside of the range of penalties. 

The \pkg{ordered} package can interpolate between the preset penalty values but cannot predict outside of their range and this will cause an error. 

We suggest that you set the collection of penalty values when fitting the model. This is important when tuning the model. Different data sets and mixture values (a.k.a. `alpha`) will have different sets of penalties and it might be good to set a wide range. 

To do this, you can use `set_engine()` to pass a vector of penalty values as so: 

```r
# Example of setting a wide penalty range
penalties <- 10^seq(-10, 0, length.out = 20)

ordinal_reg(penalty = tune()) |>
  set_engine("ordinalNet", path_values = !!penalties)
```

See [`glmnet-details`] for more background. 

## Preprocessing requirements


Factor/categorical predictors need to be converted to numeric values (e.g., dummy or indicator variables) for this engine. When using the formula method via \\code{\\link[=fit.model_spec]{fit()}}, parsnip will convert factor columns to indicators.


Predictors should have the same scale. One way to achieve this is to center and 
scale each so that each predictor has mean zero and a variance of one.

By default, [ordinalNet::ordinalNet()] uses the argument `standardize = TRUE` to center and scale the data. 

## Case weights


The underlying model implementation does not allow for case weights. 

## Prediction types


``` r
parsnip:::get_from_env("ordinal_reg_predict") |>
  dplyr::filter(engine == "ordinalNet") |>
  dplyr::select(mode, type)
```

```
## # A tibble: 2 x 2
##   mode           type 
##   <chr>          <chr>
## 1 classification class
## 2 classification prob
```

## References

- Wurm MJ, Rathouz PJ, Hanlon BM. 2021. Regularized Ordinal Regression and the ordinalNet R Package. _Journal of Statistical Software_ 99(6):1-42. \doi{10.18637/jss.v099.i06}.
