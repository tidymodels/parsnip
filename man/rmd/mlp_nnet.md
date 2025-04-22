


For this engine, there are multiple modes: classification and regression

## Tuning Parameters



This model has 3 tuning parameters:

- `hidden_units`: # Hidden Units (type: integer, default: none)

- `penalty`: Amount of Regularization (type: double, default: 0.0)

- `epochs`: # Epochs (type: integer, default: 100L)

Note that, in [nnet::nnet()], the maximum number of parameters is an argument with a fairly low value of `maxit = 1000`. For some models, you may need to pass this value in via [set_engine()] so that the model does not fail. 


## Translation from parsnip to the original package (regression)


``` r
mlp(
  hidden_units = integer(1),
  penalty = double(1),
  epochs = integer(1)
) |>  
  set_engine("nnet") |> 
  set_mode("regression") |> 
  translate()
```

```
## Single Layer Neural Network Model Specification (regression)
## 
## Main Arguments:
##   hidden_units = integer(1)
##   penalty = double(1)
##   epochs = integer(1)
## 
## Computational engine: nnet 
## 
## Model fit template:
## nnet::nnet(formula = missing_arg(), data = missing_arg(), size = integer(1), 
##     decay = double(1), maxit = integer(1), trace = FALSE, linout = TRUE)
```

Note that parsnip automatically sets linear activation in the last layer. 

## Translation from parsnip to the original package (classification)


``` r
mlp(
  hidden_units = integer(1),
  penalty = double(1),
  epochs = integer(1)
) |> 
  set_engine("nnet") |> 
  set_mode("classification") |> 
  translate()
```

```
## Single Layer Neural Network Model Specification (classification)
## 
## Main Arguments:
##   hidden_units = integer(1)
##   penalty = double(1)
##   epochs = integer(1)
## 
## Computational engine: nnet 
## 
## Model fit template:
## nnet::nnet(formula = missing_arg(), data = missing_arg(), size = integer(1), 
##     decay = double(1), maxit = integer(1), trace = FALSE, linout = FALSE)
```


## Preprocessing requirements


Factor/categorical predictors need to be converted to numeric values (e.g., dummy or indicator variables) for this engine. When using the formula method via \\code{\\link[=fit.model_spec]{fit()}}, parsnip will convert factor columns to indicators.


Predictors should have the same scale. One way to achieve this is to center and 
scale each so that each predictor has mean zero and a variance of one.

## Case weights


The underlying model implementation does not allow for case weights. 

## Saving fitted model objects


This model object contains data that are not required to make predictions. When saving the model for the purpose of prediction, the size of the saved object might be substantially reduced by using functions from the [butcher](https://butcher.tidymodels.org) package.

## Examples 

The "Fitting and Predicting with parsnip" article contains [examples](https://parsnip.tidymodels.org/articles/articles/Examples.html#mlp-nnet) for `mlp()` with the `"nnet"` engine.

## References

 - Kuhn, M, and K Johnson. 2013. _Applied Predictive Modeling_. Springer.



