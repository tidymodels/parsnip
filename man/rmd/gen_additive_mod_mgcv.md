


For this engine, there are multiple modes: regression and classification

## Tuning Parameters




This model has 2 tuning parameters:

- `select_features`: Select Features? (type: logical, default: FALSE)

- `adjust_deg_free`: Smoothness Adjustment (type: double, default: 1.0)


## Translation from parsnip to the original package  (regression)


```r
gen_additive_mod(adjust_deg_free = numeric(1), select_features = logical(1)) %>% 
  set_engine("mgcv") %>% 
  set_mode("regression") %>% 
  translate()
```

```
## GAM Model Specification (regression)
## 
## Main Arguments:
##   select_features = logical(1)
##   adjust_deg_free = numeric(1)
## 
## Computational engine: mgcv 
## 
## Model fit template:
## mgcv::gam(formula = missing_arg(), data = missing_arg(), weights = missing_arg(), 
##     select = logical(1), gamma = numeric(1))
```

## Translation from parsnip to the original package  (classification)


```r
gen_additive_mod(adjust_deg_free = numeric(1), select_features = logical(1)) %>% 
  set_engine("mgcv") %>% 
  set_mode("classification") %>% 
  translate()
```

```
## GAM Model Specification (classification)
## 
## Main Arguments:
##   select_features = logical(1)
##   adjust_deg_free = numeric(1)
## 
## Computational engine: mgcv 
## 
## Model fit template:
## mgcv::gam(formula = missing_arg(), data = missing_arg(), weights = missing_arg(), 
##     select = logical(1), gamma = numeric(1), family = stats::binomial(link = "logit"))
```

## Model fitting

This model should be used with a model formula so that smooth terms can be specified. For example:



```r
library(mgcv)
gen_additive_mod() %>% 
  set_engine("mgcv") %>% 
  set_mode("regression") %>% 
  fit(mpg ~ wt + gear + cyl + s(disp, k = 10), data = mtcars)
```

```
## parsnip model object
## 
## 
## Family: gaussian 
## Link function: identity 
## 
## Formula:
## mpg ~ wt + gear + cyl + s(disp, k = 10)
## 
## Estimated degrees of freedom:
## 7.52  total = 11.52 
## 
## GCV score: 4.225228
```

The smoothness of the terms will need to be manually specified (e.g., using `s(x, df = 10)`) in the formula. Tuning can be accomplished using the `adjust_deg_free` parameter. 


However, when using a workflow, the best approach is to avoid using [workflows::add_formula()] and use [workflows::add_variables()] in conjunction with a model formula:


```r
spec <- 
  gen_additive_mod() %>% 
  set_engine("mgcv") %>% 
  set_mode("regression")

workflow() %>% 
  add_variables(outcomes = c(mpg), predictors = c(wt, gear, cyl, disp)) %>% 
  add_model(spec, formula = mpg ~ wt + gear + cyl + s(disp, k = 10)) %>% 
  fit(data = mtcars) %>% 
  extract_fit_engine()
```

```
## 
## Family: gaussian 
## Link function: identity 
## 
## Formula:
## mpg ~ wt + gear + cyl + s(disp, k = 10)
## 
## Estimated degrees of freedom:
## 7.52  total = 11.52 
## 
## GCV score: 4.225228
```

The reason for this is that [workflows::add_formula()] will try to create the model matrix and fail to find/use `s()`.  

## Preprocessing requirements


Factor/categorical predictors need to be converted to numeric values (e.g., dummy or indicator variables) for this engine. When using the formula method via \\code{\\link[=fit.model_spec]{fit()}}, parsnip will convert factor columns to indicators.

## Case weights


This model can utilize case weights during model fitting. To use them, see the documentation in [case_weights] and the examples on `tidymodels.org`. 

The `fit()` and `fit_xy()` arguments have arguments called `case_weights` that expect vectors of case weights. 

## References

 - Ross, W. 2021. [_Generalized Additive Models in R: A Free, Interactive Course using mgcv_](https://noamross.github.io/gams-in-r-course/)
 
 - Wood, S. 2017. _Generalized Additive Models: An Introduction with R_. Chapman and Hall/CRC.
