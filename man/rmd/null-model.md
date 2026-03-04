# Engine Details

Engines may have pre-set default arguments when executing the model fit call. 
For this type of model, the template of the fit calls are below:

## parsnip


``` r
null_model() |> 
  set_engine("parsnip") |> 
  set_mode("regression") |> 
  translate()
```

```
## Null Model Specification (regression)
## 
## Computational engine: parsnip 
## 
## Model fit template:
## parsnip::nullmodel(x = missing_arg(), y = missing_arg())
```


``` r
null_model() |> 
  set_engine("parsnip") |> 
  set_mode("classification") |> 
  translate()
```

```
## Null Model Specification (classification)
## 
## Computational engine: parsnip 
## 
## Model fit template:
## parsnip::nullmodel(x = missing_arg(), y = missing_arg())
```

## Prediction types


``` r
parsnip:::get_from_env("null_model_predict") |>
  dplyr::select(mode, type)
```

```
## # A tibble: 5 x 2
##   mode           type   
##   <chr>          <chr>  
## 1 regression     numeric
## 2 regression     raw    
## 3 classification class  
## 4 classification prob   
## 5 classification raw
```
