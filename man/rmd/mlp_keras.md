


For this engine, there are multiple modes: classification and regression

## Tuning Parameters



This model has 5 tuning parameters:

- `hidden_units`: # Hidden Units (type: integer, default: 5L)

- `penalty`: Amount of Regularization (type: double, default: 0.0)

- `dropout`: Dropout Rate (type: double, default: 0.0)

- `epochs`: # Epochs (type: integer, default: 20L)

- `activation`: Activation Function (type: character, default: 'softmax')

## Translation from parsnip to the original package (regression)


``` r
mlp(
  hidden_units = integer(1),
  penalty = double(1),
  dropout = double(1),
  epochs = integer(1),
  activation = character(1)
) |>  
  set_engine("keras") |> 
  set_mode("regression") |> 
  translate()
```

```
## Single Layer Neural Network Model Specification (regression)
## 
## Main Arguments:
##   hidden_units = integer(1)
##   penalty = double(1)
##   dropout = double(1)
##   epochs = integer(1)
##   activation = character(1)
## 
## Computational engine: keras 
## 
## Model fit template:
## parsnip::keras_mlp(x = missing_arg(), y = missing_arg(), hidden_units = integer(1), 
##     penalty = double(1), dropout = double(1), epochs = integer(1), 
##     activation = character(1))
```

## Translation from parsnip to the original package (classification)


``` r
mlp(
  hidden_units = integer(1),
  penalty = double(1),
  dropout = double(1),
  epochs = integer(1),
  activation = character(1)
) |> 
  set_engine("keras") |> 
  set_mode("classification") |> 
  translate()
```

```
## Single Layer Neural Network Model Specification (classification)
## 
## Main Arguments:
##   hidden_units = integer(1)
##   penalty = double(1)
##   dropout = double(1)
##   epochs = integer(1)
##   activation = character(1)
## 
## Computational engine: keras 
## 
## Model fit template:
## parsnip::keras_mlp(x = missing_arg(), y = missing_arg(), hidden_units = integer(1), 
##     penalty = double(1), dropout = double(1), epochs = integer(1), 
##     activation = character(1))
```


## Preprocessing requirements


Factor/categorical predictors need to be converted to numeric values (e.g., dummy or indicator variables) for this engine. When using the formula method via \\code{\\link[=fit.model_spec]{fit()}}, parsnip will convert factor columns to indicators.


Predictors should have the same scale. One way to achieve this is to center and 
scale each so that each predictor has mean zero and a variance of one.

## Case weights


The underlying model implementation does not allow for case weights. 

## Saving fitted model objects


Models fitted with this engine may require native serialization methods to be properly saved and/or passed between R sessions. To learn more about preparing fitted models for serialization, see the bundle package.

## Examples 

The "Fitting and Predicting with parsnip" article contains [examples](https://parsnip.tidymodels.org/articles/articles/Examples.html#mlp-keras) for `mlp()` with the `"keras"` engine.

## References

 - Kuhn, M, and K Johnson. 2013. _Applied Predictive Modeling_. Springer.


