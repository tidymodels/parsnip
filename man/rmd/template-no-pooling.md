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

