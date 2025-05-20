


For this engine, there is a single mode: censored regression

## Tuning Parameters



This model has 1 tuning parameters:

- `dist`: Distribution (type: character, default: 'weibull')

## Translation from parsnip to the original package

The **censored** extension package is required to fit this model.


``` r
library(censored)

survival_reg(dist = character(1)) |> 
  set_engine("flexsurv") |> 
  set_mode("censored regression") |> 
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

For this engine, stratification cannot be specified via [`survival::strata()`], please see [flexsurv::flexsurvreg()] for alternative specifications.



Predictions of type `"time"` are predictions of the mean survival time.

## Case weights


This model can utilize case weights during model fitting. To use them, see the documentation in [case_weights] and the examples on `tidymodels.org`. 

The `fit()` and `fit_xy()` arguments have arguments called `case_weights` that expect vectors of case weights. 

## Saving fitted model objects


This model object contains data that are not required to make predictions. When saving the model for the purpose of prediction, the size of the saved object might be substantially reduced by using functions from the [butcher](https://butcher.tidymodels.org) package.


## References

- Jackson, C. 2016. `flexsurv`: A Platform for Parametric Survival  Modeling in R. _Journal of Statistical Software_, 70(8), 1 - 33.
