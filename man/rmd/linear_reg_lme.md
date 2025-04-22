


For this engine, there is a single mode: regression

## Tuning Parameters

This model has no tuning parameters.

## Translation from parsnip to the original package

The **multilevelmod** extension package is required to fit this model.


``` r
library(multilevelmod)

linear_reg() |> 
  set_engine("lme") |> 
  set_mode("regression") |> 
  translate()
```

```
## Linear Regression Model Specification (regression)
## 
## Computational engine: lme 
## 
## Model fit template:
## nlme::lme(fixed = missing_arg(), data = missing_arg())
```


## Predicting new samples

This model can use subject-specific coefficient estimates to make predictions (i.e. partial pooling). For example, this equation shows the linear predictor (`\eta`) for a random intercept: 

```
\eta_{i} = (\beta_0 + b_{0i}) + \beta_1x_{i1}
```

where `i` denotes the `i`th independent experimental unit (e.g. subject). When the model has seen subject `i`, it can use that subject's data to adjust the _population_ intercept to be more specific to that subjects results. 

What happens when data are being predicted for a subject that was not used in the model fit? In that case, this package uses _only_ the population parameter estimates for prediction: 

```
\hat{\eta}_{i'} = \hat{\beta}_0+ \hat{\beta}x_{i'1}
```

Depending on what covariates are in the model, this might have the effect of making the same prediction for all new samples. The population parameters are the "best estimate" for a subject that was not included in the model fit.  

The tidymodels framework deliberately constrains predictions for new data to not use the training set or other data (to prevent information leakage). 


## Preprocessing requirements

There are no specific preprocessing needs. However, it is helpful to keep the clustering/subject identifier column as factor or character (instead of making them into dummy variables). See the examples in the next section. 

## Other details

The model can accept case weights. 

With parsnip, we suggest using the _fixed effects_ formula method when fitting, but the random effects formula should be passed to `set_engine()` since it is an irregular (but required) argument:

```r
library(tidymodels)
data("riesby")

linear_reg() |> 
  set_engine("lme", random =  ~ 1|subject) |> 
  fit(depr_score ~ week, data = riesby)
```

When using tidymodels infrastructure, it may be better to use a workflow. In this case, you can add the appropriate columns using `add_variables()` then supply the typical formula when adding the model: 

```r
library(tidymodels)

lme_spec <- 
  linear_reg() |> 
  set_engine("lme", random =  ~ 1|subject)

lme_wflow <- 
  workflow() |> 
  # The data are included as-is using:
  add_variables(outcomes = depr_score, predictors = c(week, subject)) |> 
  add_model(lme_spec, formula = depr_score ~ week)

fit(lme_wflow, data = riesby)
```

## Case weights


The underlying model implementation does not allow for case weights. 

## References

- J Pinheiro, and D Bates. 2000. _Mixed-effects models in S and S-PLUS_. Springer, New York, NY
 
- West, K, Band Welch, and A Galecki. 2014. _Linear Mixed Models: A Practical Guide Using Statistical Software_. CRC Press.
 
- Thorson, J, Minto, C. 2015, Mixed effects: a unifying framework for statistical modelling in fisheries biology. _ICES Journal of Marine Science_, Volume 72, Issue 5, Pages 1245–1256.
  
- Harrison, XA, Donaldson, L, Correa-Cano, ME, Evans, J, Fisher, DN, Goodwin, CED, Robinson, BS, Hodgson, DJ, Inger, R. 2018. _A brief introduction to mixed effects modelling and multi-model inference in ecology_. PeerJ 6:e4794. 
  
- DeBruine LM, Barr DJ. Understanding Mixed-Effects Models Through Data Simulation. 2021. _Advances in Methods and Practices in Psychological Science_.   
  
