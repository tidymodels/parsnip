


For this engine, there are multiple modes: classification and regression

## Tuning Parameters



This model has 6 tuning parameters:

- `hidden_units`: # Hidden Units (type: integer, default: 200L)

- `penalty`: Amount of Regularization (type: double, default: 0.0)

- `dropout`: Dropout Rate (type: double, default: 0.5)

- `epochs`: # Epochs (type: integer, default: 10)

- `activation`: Activation function (type: character, default: 'see below')

- `learn_rate`: Learning Rate (type: double, default: 0.005)

The naming of activation functions in [h2o::h2o.deeplearning()] differs from parsnip's conventions. Currently, only "relu" and "tanh" are supported and will be converted internally to "Rectifier" and "Tanh" passed to the fitting function.

`penalty` corresponds to l2 penalty. [h2o::h2o.deeplearning()] also supports specifying the l1 penalty directly with the engine argument `l1`. 

Other engine arguments of interest: 

- `stopping_rounds` controls early stopping rounds based on the convergence of another engine parameter `stopping_metric`. By default, [h2o::h2o.deeplearning] stops training if simple moving average of length 5 of the stopping_metric does not improve for 5 scoring events.  This is mostly useful when used alongside the engine parameter `validation`, which is the __proportion__ of train-validation split, parsnip will split and pass the two data frames to h2o. Then [h2o::h2o.deeplearning] will evaluate the metric and early stopping criteria on the validation set. 

- h2o uses a 50% dropout ratio controlled by `dropout` for hidden layers by default. [h2o::h2o.deeplearning()] provides an engine argument `input_dropout_ratio` for dropout ratios in the input layer, which defaults to 0. 


## Translation from parsnip to the original package (regression)


[agua::h2o_train_mlp] is a wrapper around [h2o::h2o.deeplearning()].


``` r
mlp(
  hidden_units = integer(1),
  penalty = double(1),
  dropout = double(1),
  epochs = integer(1),
  learn_rate = double(1),
  activation = character(1)
) |>  
  set_engine("h2o") |> 
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
## Computational engine: h2o 
## 
## Model fit template:
## agua::h2o_train_mlp(x = missing_arg(), y = missing_arg(), weights = missing_arg(), 
##     validation_frame = missing_arg(), hidden = integer(1), l2 = double(1), 
##     hidden_dropout_ratios = double(1), epochs = integer(1), activation = character(1), 
##     rate = double(1))
```

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
  set_engine("h2o") |> 
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
## Computational engine: h2o 
## 
## Model fit template:
## agua::h2o_train_mlp(x = missing_arg(), y = missing_arg(), weights = missing_arg(), 
##     validation_frame = missing_arg(), hidden = integer(1), l2 = double(1), 
##     hidden_dropout_ratios = double(1), epochs = integer(1), activation = character(1), 
##     rate = double(1))
```


## Preprocessing requirements


Factor/categorical predictors need to be converted to numeric values (e.g., dummy or indicator variables) for this engine. When using the formula method via \\code{\\link[=fit.model_spec]{fit()}}, parsnip will convert factor columns to indicators.


Predictors should have the same scale. One way to achieve this is to center and 
scale each so that each predictor has mean zero and a variance of one.


By default, [h2o::h2o.deeplearning()] uses the argument `standardize = TRUE` to center and scale all numeric columns. 


## Initializing h2o 


To use the h2o engine with tidymodels, please run `h2o::h2o.init()` first. By default, This connects R to the local h2o server. This needs to be done in every new R session. You can also connect to a remote h2o server with an IP address, for more details see [h2o::h2o.init()]. 

You can control the number of threads in the thread pool used by h2o with the `nthreads` argument. By default, it uses all CPUs on the host. This is different from the usual parallel processing mechanism in tidymodels for tuning, while tidymodels parallelizes over resamples, h2o parallelizes over hyperparameter combinations for a given resample. 

h2o will automatically shut down the local h2o instance started by R when R is terminated. To manually stop the h2o server, run `h2o::h2o.shutdown()`. 

## Saving fitted model objects


Models fitted with this engine may require native serialization methods to be properly saved and/or passed between R sessions. To learn more about preparing fitted models for serialization, see the bundle package.
