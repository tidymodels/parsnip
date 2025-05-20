# Engine Details




Engines may have pre-set default arguments when executing the model fit call. For this type of model, the template of the fit calls are below:

## kknn


```r
nearest_neighbor() |> 
  set_engine("kknn") |> 
  set_mode("regression") |> 
  translate()
```

```
## K-Nearest Neighbor Model Specification (regression)
## 
## Computational engine: kknn 
## 
## Model fit template:
## kknn::train.kknn(formula = missing_arg(), data = missing_arg(), 
##     ks = min_rows(5, data, 5))
```


```r
nearest_neighbor() |> 
  set_engine("kknn") |> 
  set_mode("classification") |> 
  translate()
```

```
## K-Nearest Neighbor Model Specification (classification)
## 
## Computational engine: kknn 
## 
## Model fit template:
## kknn::train.kknn(formula = missing_arg(), data = missing_arg(), 
##     ks = min_rows(5, data, 5))
```

For `kknn`, the underlying modeling function used is a restricted version of
`train.kknn()` and not `kknn()`. It is set up in this way so that parsnip can
utilize the underlying `predict.train.kknn` method to predict on new data. This
also means that a single value of that function's `kernel` argument (a.k.a
`weight_func` here) can be supplied

For this engine, tuning over `neighbors` is very efficient since the same model 
object can be used to make predictions over multiple values of `neighbors`. 

## Parameter translations

The standardized parameter names in parsnip can be mapped to their original 
names in each engine that has main parameters. Each engine typically has a 
different default value (shown in parentheses) for each parameter.


|**parsnip** |**kknn**         |
|:-----------|:----------------|
|neighbors   |ks               |
|weight_func |kernel (optimal) |
|dist_power  |distance (2)     |

