


For this engine, there is a single mode: regression

## Tuning Parameters

This model has no formal tuning parameters. It may be beneficial to determine the appropriate correlation structure to use, but this typically does not affect the predicted value of the model. It _does_ have an effect on the inferential results and parameter covariance values. 

## Translation from parsnip to the original package

The **multilevelmod** extension package is required to fit this model.


```r
library(multilevelmod)

poisson_reg(engine = "gee") %>% 
  set_engine("gee") %>% 
  translate()
```

```
## Poisson Regression Model Specification (regression)
## 
## Computational engine: gee 
## 
## Model fit template:
## multilevelmod::gee_fit(formula = missing_arg(), data = missing_arg(), 
##     family = stats::poisson)
```

`multilevelmod::gee_fit()` is a wrapper model around `gee()`. 


## Preprocessing requirements

There are no specific preprocessing needs. However, it is helpful to keep the clustering/subject identifier column as factor or character (instead of making them into dummy variables). See the examples in the next section. 

## Case weights


The underlying model implementation does not allow for case weights. 

## Other details

Both `gee:gee()` and `gee:geepack()` specify the id/cluster variable using an argument `id` that requires a vector. parsnip doesn't work that way so we enable this model to be fit using a artificial function `id_var()` to be used in the formula. So, in the original package, the call would look like:

```r
gee(breaks ~ tension, id = wool, data = warpbreaks, corstr = "exchangeable")
```

With parsnip, we suggest using the formula method when fitting: 

```r
library(tidymodels)

poisson_reg() %>% 
  set_engine("gee", corstr = "exchangeable") %>% 
  fit(y ~ time + x + id_var(subject), data = longitudinal_counts)
```

When using tidymodels infrastructure, it may be better to use a workflow. In this case, you can add the appropriate columns using `add_variables()` then supply the GEE formula when adding the model: 

```r
library(tidymodels)

gee_spec <- 
  poisson_reg() %>% 
  set_engine("gee", corstr = "exchangeable")

gee_wflow <- 
  workflow() %>% 
  # The data are included as-is using:
  add_variables(outcomes = y, predictors = c(time, x, subject)) %>% 
  add_model(gee_spec, formula = y ~ time + x + id_var(subject))

fit(gee_wflow, data = longitudinal_counts)
```

The `gee::gee()` function always prints out warnings and output even when `silent = TRUE`. The parsnip `"gee"` engine, by contrast, silences all console output coming from `gee::gee()`, even if `silent = FALSE`.

Also, because of issues with the `gee()` function, a supplementary call to `glm()` is needed to get the rank and QR decomposition objects so that `predict()` can be used.

## References

 - Liang, K.Y. and Zeger, S.L. (1986) Longitudinal data analysis using generalized linear models. _Biometrika_, 73 13–22.

 - Zeger, S.L. and Liang, K.Y. (1986) Longitudinal data analysis for discrete and continuous outcomes. _Biometrics_, 42 121–130.

