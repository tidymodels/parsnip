


For this engine, there are multiple modes: classification and regression

## Tuning Parameters



This model has 7 tuning parameters:

- `epochs`: # Epochs (type: integer, default: 100L)

- `hidden_units`: # Hidden Units (type: integer, default: 3L)

- `activation`: Activation Function (type: character, default: 'relu')

- `penalty`: Amount of Regularization (type: double, default: 0.001)

- `mixture`: Proportion of Lasso Penalty (type: double, default: 0.0)

- `dropout`: Dropout Rate (type: double, default: 0.0)

- `learn_rate`: Learning Rate (type: double, default: 0.01)

The use of the L1 penalty (a.k.a. the lasso penalty) does _not_ force parameters to be strictly zero (as it does in packages such as glmnet). The zeroing out of parameters is a specific feature the optimization method used in those packages.

Both `penalty` and `dropout` should be not be used in the same model. 

Other engine arguments of interest: 

 - `hidden_layer_2` and `activation_2` control the format of the second layer. 
 - `momentum`: A number used to use historical gradient information during optimization.
 - `batch_size`: An integer for the number of training set points in each batch.
 - `class_weights`: Numeric class weights. See [brulee::brulee_mlp()].
 - `stop_iter`: A non-negative integer for how many iterations with no improvement before stopping. (default: 5L).
 - `rate_schedule`: A function to change the learning rate over epochs. See [brulee::schedule_decay_time()] for details. 


## Translation from parsnip to the original package (regression)


``` r
mlp(
  hidden_units = integer(1),
  penalty = double(1),
  dropout = double(1),
  epochs = integer(1),
  learn_rate = double(1),
  activation = character(1)
) |>
  set_engine("brulee_two_layer",
             hidden_units_2 = integer(1),
             activation_2 = character(1)) |> 
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
##   learn_rate = double(1)
## 
## Engine-Specific Arguments:
##   hidden_units_2 = integer(1)
##   activation_2 = character(1)
## 
## Computational engine: brulee_two_layer 
## 
## Model fit template:
## brulee::brulee_mlp_two_layer(x = missing_arg(), y = missing_arg(), 
##     hidden_units = integer(1), penalty = double(1), dropout = double(1), 
##     epochs = integer(1), activation = character(1), learn_rate = double(1), 
##     hidden_units_2 = integer(1), activation_2 = character(1))
```

Note that parsnip automatically sets the linear activation in the last layer. 

## Translation from parsnip to the original package (classification)


``` r
mlp(
  hidden_units = integer(1),
  penalty = double(1),
  dropout = double(1),
  epochs = integer(1),
  learn_rate = double(1),
  activation = character(1)
) |> 
  set_engine("brulee_two_layer",
             hidden_units_2 = integer(1),
             activation_2 = character(1)) |> 
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
##   learn_rate = double(1)
## 
## Engine-Specific Arguments:
##   hidden_units_2 = integer(1)
##   activation_2 = character(1)
## 
## Computational engine: brulee_two_layer 
## 
## Model fit template:
## brulee::brulee_mlp_two_layer(x = missing_arg(), y = missing_arg(), 
##     hidden_units = integer(1), penalty = double(1), dropout = double(1), 
##     epochs = integer(1), activation = character(1), learn_rate = double(1), 
##     hidden_units_2 = integer(1), activation_2 = character(1))
```


## Preprocessing requirements


Factor/categorical predictors need to be converted to numeric values (e.g., dummy or indicator variables) for this engine. When using the formula method via \\code{\\link[=fit.model_spec]{fit()}}, parsnip will convert factor columns to indicators.


Predictors should have the same scale. One way to achieve this is to center and 
scale each so that each predictor has mean zero and a variance of one.

## Case weights


The underlying model implementation does not allow for case weights. 

## References

 - Kuhn, M, and K Johnson. 2013. _Applied Predictive Modeling_. Springer.



