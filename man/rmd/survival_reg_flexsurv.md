


For this engine, there is a single mode: censored regression

## Tuning Parameters



This model has 1 tuning parameters:

- `dist`: Distribution (type: character, default: 'weibull')

## Translation from parsnip to the original package

The **censored** extension package is required to fit this model.


```r
library(censored)

survival_reg(dist = character(1)) %>% 
  set_engine("flexsurv") %>% 
  set_mode("censored regression") %>% 
  translate()
```

```
## Parametric Survival Regression Model Specification (censored regression)
## 
## Main Arguments:
##   dist = character(1)
## 
## Computational engine: flexsurv 
## 
## Model fit template:
## flexsurv::flexsurvreg(formula = missing_arg(), data = missing_arg(), 
##     weights = missing_arg(), dist = character(1))
```

## Other details

The main interface for this model uses the formula method since the model specification typically involved the use of [survival::Surv()]. 

For this engine, stratification cannot be specified via [`strata()`], please see [flexsurv::flexsurvreg()] for alternative specifications.



Predictions of type `"time"` are predictions of the mean survival time.

## References

- Jackson, C. 2016. `flexsurv`: A Platform for Parametric Survival  Modeling in R. _Journal of Statistical Software_, 70(8), 1 - 33.
