


For this engine, there is a single mode: quantile regression

## Tuning Parameters



This model has 4 tuning parameters:

- `hidden_units`: # Hidden Units (type: integer, default: 2L)

- `penalty`: Amount of Regularization (type: double, default: 0.0)

- `epochs`: # Epochs (type: integer, default: 5000L)

- `activation`: Activation Function (type: character, default: 'sigmoid')

Other engine arguments of interest: 

 - `n.trials`: number of repeated trials used to avoid local minima.
 - `method`: The optimization technique (`"nlm"` or `"adam"`).

## Translation from parsnip to the original package (quantile regression)


``` r
mlp(
  hidden_units = integer(1),
  penalty = double(1),
  epochs = integer(1),
  activation = character(1)
) |>  
  set_engine("qrnn") |> 
  set_mode("quantile regression", quantile_levels = (1:3) / 4) |> 
  translate()
```

```
## Single Layer Neural Network Model Specification (quantile regression)
## 
## Main Arguments:
##   hidden_units = integer(1)
##   penalty = double(1)
##   epochs = integer(1)
##   activation = character(1)
## 
## Computational engine: qrnn 
## 
## Model fit template:
## parsnip::mcqrnn_train(x = missing_arg(), y = missing_arg(), n.hidden = integer(1), 
##     penalty = double(1), iter.max = integer(1), Th = character(1), 
##     trace = FALSE, tau = quantile_levels)
```

```
## Quantile levels: 0.25, 0.5, and 0.75.
```

## Preprocessing requirements


Factor/categorical predictors need to be converted to numeric values (e.g., dummy or indicator variables) for this engine. When using the formula method via \\code{\\link[=fit.model_spec]{fit()}}, parsnip will convert factor columns to indicators.


Predictors should have the same scale. One way to achieve this is to center and 
scale each so that each predictor has mean zero and a variance of one.

## Case weights


The underlying model implementation does not allow for case weights. 

## Prediction types


``` r
parsnip:::get_from_env("mlp_predict") |>
  dplyr::filter(engine == "qrnn") |>
  dplyr::select(mode, type)
```

```
## # A tibble: 1 x 2
##   mode                type    
##   <chr>               <chr>   
## 1 quantile regression quantile
```

## References

 - Cannon, A.J., 2018. Non-crossing nonlinear regression quantiles by monotone composite quantile regression neural network, with application to rainfall extremes. _Stochastic Environmental Research and Risk Assessment_, 32(11): 3207-3225. doi:10.1007/s00477-018-1573-6



