


For this engine, there are multiple modes: classification and regression

## Tuning Parameters



This model has 6 tuning parameters:

- `hidden_units`: # Hidden Units (type: integer, default: 3L)

- `penalty`: Amount of Regularization (type: double, default: 0.001)

- `epochs`: # Epochs (type: integer, default: 100L)

- `dropout`: Dropout Rate (type: double, default: 0.0)

- `learn_rate`: Learning Rate (type: double, default: 0.01)

- `activation`: Activation Function (type: character, default: 'relu')

The use of the L1 penalty (a.k.a. the lasso penalty) does _not_ force parameters to be strictly zero (as it does in packages such as glmnet). The zeroing out of parameters is a specific feature the optimization method used in those packages.

Both `penalty` and `dropout` should be not be used in the same model. 

Other engine arguments of interest: 

 - `momentum()`: A number used to use historical gradient infomration during optimization.
 - `batch_size()`: An integer for the number of training set points in each batch.
 - `class_weights()`: Numeric class weights. See [brulee::brulee_mlp()].
 - `stop_iter()`: A non-negative integer for how many iterations with no improvement before stopping. (default: 5L).

Parsnip changes the default range for `learn_rate` to `c(-2.5, -0.5)`.

## Translation from parsnip to the original package (regression)


``` r
mlp(
  hidden_units = integer(1),
  penalty = double(1),
  dropout = double(1),
  epochs = integer(1),
  learn_rate = double(1),
  activation = character(1)
) %>%  
  set_engine("brulee") %>% 
  set_mode("regression") %>% 
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
## Computational engine: brulee 
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
) %>% 
  set_engine("brulee") %>% 
  set_mode("classification") %>% 
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
## Computational engine: brulee 
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

## References

 - Kuhn, M, and K Johnson. 2013. _Applied Predictive Modeling_. Springer.



