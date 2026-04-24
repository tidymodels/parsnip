


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

ResNet models are almost always multilayer. They are similar to multi-layer neural networks, but typically have two differences: 

 - At the end of some blocks of nonlinear activation, the features available at the beginning of the block are _added_ to the features at the end of the block. 
 - Batch normalization is used before some hidden layers to center and scale the features prior to activation. Normally, the number of features produced by batch normalization equals the number of incoming hidden units. In brulee's implementation, this number of "batch units" must only be greater than 1.

Two important vector-based model parameters are: 

 - `batch_norm_units`, a vector the same length as `hidden_units`, specifies the number of outgoing features from the normalization. 
 - `residual_at` is a vector that specifies which elements of `hidden_units` should be followed by the addition operator, so that the skip later should occur. 

Other engine arguments of interest: 

 - `momentum`: A number used to use historical gradient infomration during optimization.
 - `batch_size`: An integer for the number of training set points in each batch.
 - `class_weights`: Numeric class weights. See [brulee::brulee_resnet()].
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
  set_engine("brulee_resnet") |> 
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
## Computational engine: brulee_resnet 
## 
## Model fit template:
## brulee::brulee_mlp(x = missing_arg(), y = missing_arg(), hidden_units = integer(1), 
##     penalty = double(1), dropout = double(1), epochs = integer(1), 
##     activation = character(1), learn_rate = double(1))
```

Note that parsnip automatically sets linear activation in the last layer. 

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
  set_engine("brulee_resnet") |> 
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
## Computational engine: brulee_resnet 
## 
## Model fit template:
## brulee::brulee_mlp(x = missing_arg(), y = missing_arg(), hidden_units = integer(1), 
##     penalty = double(1), dropout = double(1), epochs = integer(1), 
##     activation = character(1), learn_rate = double(1))
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
  dplyr::filter(engine == "brulee_resnet") |>
  dplyr::select(mode, type)
```

```
## # A tibble: 3 x 2
##   mode           type   
##   <chr>          <chr>  
## 1 regression     numeric
## 2 classification class  
## 3 classification prob
```

## References
 
 - Equation 2 of Gorishniy, Y., Rubachev, I., Khrulkov, V., & Babenko, A. (2021). Revisiting deep learning models for tabular data. _Advances in neural information processing systems_, 34, 18932-18943.





