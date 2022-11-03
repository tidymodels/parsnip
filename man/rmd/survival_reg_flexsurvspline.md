


For this engine, there is a single mode: censored regression

## Tuning Parameters

This model has one engine-specific tuning parameter:

 * `k`: Number of knots in the spline. The default is `k = 0`.

## Translation from parsnip to the original package

The **censored** extension package is required to fit this model.


```r
library(censored)

survival_reg() %>% 
  set_engine("flexsurvspline") %>% 
  set_mode("censored regression") %>% 
  translate()
```

```
## Parametric Survival Regression Model Specification (censored regression)
## 
## Computational engine: flexsurvspline 
## 
## Model fit template:
## flexsurv::flexsurvspline(formula = missing_arg(), data = missing_arg(), 
##     weights = missing_arg())
```

## Other details

The main interface for this model uses the formula method since the model specification typically involved the use of [survival::Surv()]. 

For this engine, stratification cannot be specified via [`strata()`], please see [flexsurv::flexsurvspline()] for alternative specifications.



Predictions of type `"time"` are predictions of the mean survival time.

## Saving fitted model objects


This model object contains data that are not required to make predictions. When saving the model for the purpose of prediction, the size of the saved object might be substantially reduced by using functions from the [butcher](https://butcher.tidymodels.org) package.


## References

- Jackson, C. 2016. `flexsurv`: A Platform for Parametric Survival  Modeling in R. _Journal of Statistical Software_, 70(8), 1 - 33.
