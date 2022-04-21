


For this engine, there is a single mode: regression

## Tuning Parameters

This model has no tuning parameters.

## Translation from parsnip to the original package

The **multilevelmod** extension package is required to fit this model.


```r
library(multilevelmod)

linear_reg() %>% 
  set_engine("gls") %>% 
  set_mode("regression") %>% 
  translate()
```

```
## Linear Regression Model Specification (regression)
## 
## Computational engine: gls 
## 
## Model fit template:
## nlme::gls(model = missing_arg(), data = missing_arg())
```


## Preprocessing requirements

There are no specific preprocessing needs. However, it is helpful to keep the clustering/subject identifier column as factor or character (instead of making them into dummy variables). See the examples in the next section. 

## Other details

The model can accept case weights. 

With parsnip, we suggest using the _fixed effects_ formula method when fitting, but the details of the correlation structure should be passed to `set_engine()` since it is an irregular (but required) argument:


```r
library(tidymodels)
# load nlme to be able to use the `cor*()` functions
library(nlme)

data("riesby")

linear_reg() %>% 
  set_engine("gls", correlation =  corCompSymm(form = ~ 1 | subject)) %>% 
  fit(depr_score ~ week, data = riesby)
```

```
## parsnip model object
## 
## Generalized least squares fit by REML
##   Model: depr_score ~ week 
##   Data: data 
##   Log-restricted-likelihood: -765.0148
## 
## Coefficients:
## (Intercept)        week 
##   -4.953439   -2.119678 
## 
## Correlation Structure: Compound symmetry
##  Formula: ~1 | subject 
##  Parameter estimate(s):
##       Rho 
## 0.6820145 
## Degrees of freedom: 250 total; 248 residual
## Residual standard error: 6.868785
```

When using tidymodels infrastructure, it may be better to use a workflow. In this case, you can add the appropriate columns using `add_variables()` then supply the typical formula when adding the model: 

```r
library(tidymodels)

gls_spec <- 
  linear_reg() %>% 
  set_engine("gls", correlation =  corCompSymm(form = ~ 1 | subject))

gls_wflow <- 
  workflow() %>% 
  # The data are included as-is using:
  add_variables(outcomes = depr_score, predictors = c(week, subject)) %>% 
  add_model(gls_spec, formula = depr_score ~ week)

fit(gls_wflow, data = riesby)
```

# Degrees of freedom

Note that [nlme::lme()] and [nlme::gls()] can fit the same model but will count degrees of freedom differently. If there are `n` data points, `p` fixed effects parameters, and `q` random effect parameters, the residual degrees of freedom are:

* **lme**: n - p - q
* **gls**: n - p

As a result, p-values will be different. For example, we can fit the same model using different estimation methods (assuming a positive covariance value): 


```r
gls_fit <- 
  linear_reg() %>% 
  set_engine("gls", correlation =  corCompSymm(form = ~ 1 | subject)) %>% 
  fit(depr_score ~ week, data = riesby)

lme_fit <-
  linear_reg() %>% 
  set_engine("lme", random =  ~ 1 | subject) %>% 
  fit(depr_score ~ week, data = riesby)
```

The estimated within-subject correlations are the same:


```r
library(ape)

# lme, use ape package:
lme_within_sub <- varcomp(lme_fit$fit)/sum(varcomp(lme_fit$fit))
lme_within_sub["subject"]
```

```
##   subject 
## 0.6820145
```

```r
# gls:
summary(gls_fit$fit$modelStruct)
```

```
## Correlation Structure: Compound symmetry
##  Formula: ~1 | subject 
##  Parameter estimate(s):
##       Rho 
## 0.6820145
```

as are the fixed effects (and their standard errors):


```r
nlme::fixef(lme_fit$fit)
```

```
## (Intercept)        week 
##   -4.953439   -2.119678
```

```r
coef(gls_fit$fit)
```

```
## (Intercept)        week 
##   -4.953439   -2.119678
```

However, the p-values for the fixed effects are different:



```r
library(broom.mixed)

# lme:
lme_fit %>% 
  tidy() %>% 
  dplyr::filter(is.na(group)) %>% 
  dplyr::select(-group, -effect)
```

```
## # A tibble: 2 × 6
##   term        estimate std.error    df statistic  p.value
##   <chr>          <dbl>     <dbl> <dbl>     <dbl>    <dbl>
## 1 (Intercept)    -4.95     0.808   183     -6.13 5.37e- 9
## 2 week           -2.12     0.224   183     -9.47 1.41e-17
```

```r
# gls:
gls_fit %>% 
  tidy()
```

```
## # A tibble: 2 × 5
##   term        estimate std.error statistic  p.value
##   <chr>          <dbl>     <dbl>     <dbl>    <dbl>
## 1 (Intercept)    -4.95     0.808     -6.13 3.50e- 9
## 2 week           -2.12     0.224     -9.47 2.26e-18
```



## References

- J Pinheiro, and D Bates. 2000. _Mixed-effects models in S and S-PLUS_. Springer, New York, NY
 
