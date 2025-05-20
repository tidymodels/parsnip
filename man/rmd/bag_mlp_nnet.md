


For this engine, there are multiple modes: classification and regression

## Tuning Parameters



This model has 3 tuning parameters:

- `hidden_units`: # Hidden Units (type: integer, default: 10L)

- `penalty`: Amount of Regularization (type: double, default: 0.0)

- `epochs`: # Epochs (type: integer, default: 1000L)

These defaults are set by the `baguette` package and are different than those in [nnet::nnet()]. 

## Translation from parsnip to the original package (classification)

The **baguette** extension package is required to fit this model.


``` r
library(baguette)

bag_mlp(penalty = double(1), hidden_units = integer(1)) |> 
  set_engine("nnet") |> 
  set_mode("classification") |> 
  translate()
```

```
## Bagged Neural Network Model Specification (classification)
## 
## Main Arguments:
##   hidden_units = integer(1)
##   penalty = double(1)
## 
## Computational engine: nnet 
## 
## Model fit template:
## baguette::bagger(formula = missing_arg(), data = missing_arg(), 
##     weights = missing_arg(), size = integer(1), decay = double(1), 
##     base_model = "nnet")
```


## Translation from parsnip to the original package (regression)

The **baguette** extension package is required to fit this model.


``` r
library(baguette)

bag_mlp(penalty = double(1), hidden_units = integer(1)) |> 
  set_engine("nnet") |> 
  set_mode("regression") |> 
  translate()
```

```
## Bagged Neural Network Model Specification (regression)
## 
## Main Arguments:
##   hidden_units = integer(1)
##   penalty = double(1)
## 
## Computational engine: nnet 
## 
## Model fit template:
## baguette::bagger(formula = missing_arg(), data = missing_arg(), 
##     weights = missing_arg(), size = integer(1), decay = double(1), 
##     base_model = "nnet")
```


## Preprocessing requirements


Factor/categorical predictors need to be converted to numeric values (e.g., dummy or indicator variables) for this engine. When using the formula method via \\code{\\link[=fit.model_spec]{fit()}}, parsnip will convert factor columns to indicators.


Predictors should have the same scale. One way to achieve this is to center and 
scale each so that each predictor has mean zero and a variance of one.

## Case weights


The underlying model implementation does not allow for case weights. 


## References

 - Breiman L. 1996. "Bagging predictors". Machine Learning. 24 (2): 123-140

 - Kuhn, M, and K Johnson. 2013. *Applied Predictive Modeling*. Springer.
