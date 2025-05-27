


For this engine, there is a single mode: classification

## Tuning Parameters



This model has 2 tuning parameter:

- `penalty`: Amount of Regularization (type: double, default: 0.001)

- `mixture`: Proportion of Lasso Penalty (type: double, default: 0.0)

The use of the L1 penalty (a.k.a. the lasso penalty) does _not_ force parameters to be strictly zero (as it does in packages such as glmnet). The zeroing out of parameters is a specific feature the optimization method used in those packages.

Other engine arguments of interest: 

 - `optimizer()`: The optimization method. See [brulee::brulee_linear_reg()].
 - `epochs()`: An integer for the number of passes through the training set. 
 - `lean_rate()`: A number used to accelerate the gradient decsent process. 
 - `momentum()`: A number used to use historical gradient information during optimization  (`optimizer = "SGD"` only).
 - `batch_size()`: An integer for the number of training set points in each batch.
 - `stop_iter()`: A non-negative integer for how many iterations with no improvement before stopping. (default: 5L).
 - `class_weights()`: Numeric class weights. See [brulee::brulee_multinomial_reg()].


## Translation from parsnip to the original package (classification)


``` r
multinom_reg(penalty = double(1)) |> 
  set_engine("brulee") |> 
  translate()
```

```
## Multinomial Regression Model Specification (classification)
## 
## Main Arguments:
##   penalty = double(1)
## 
## Computational engine: brulee 
## 
## Model fit template:
## brulee::brulee_multinomial_reg(x = missing_arg(), y = missing_arg(), 
##     penalty = double(1))
```



Factor/categorical predictors need to be converted to numeric values (e.g., dummy or indicator variables) for this engine. When using the formula method via \\code{\\link[=fit.model_spec]{fit()}}, parsnip will convert factor columns to indicators.


Predictors should have the same scale. One way to achieve this is to center and 
scale each so that each predictor has mean zero and a variance of one.

## Case weights


The underlying model implementation does not allow for case weights. 

## References

 - Kuhn, M, and K Johnson. 2013. _Applied Predictive Modeling_. Springer.
