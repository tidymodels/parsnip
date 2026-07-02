


For this engine, there are multiple modes: 

## Tuning Parameters



This model has 0 tuning parameters:



The use of the L1 penalty (a.k.a. the lasso penalty) does _not_ force parameters to be strictly zero (as it does in packages such as glmnet). The zeroing out of parameters is a specific feature the optimization method used in those packages.

## Translation from parsnip to the original package (regression)


``` r
tabular_auto_int(
  epochs = integer(1),
  num_embedding = integer(1),
  hidden_units = NULL,
  hidden_activations = NULL,
  num_attn_feat = integer(1),
  num_attn_heads = integer(1),
  num_attn_blocks = integer(1),
  activation = character(1),
  dropout = double(1),
  dropout_attn = double(1),
  dropout_embedding = double(1),
  penalty = double(1),
  mixture = double(1),
  learn_rate = double(1),
  rate_schedule = tune(),
  momentum = character(1),
  batch_size = NULL,
  class_weights = NULL,
  stop_iter = integer(1)
  ) |>  
  set_engine("brulee") |> 
  set_mode("regression") |> 
  translate()
```

```
## ! parsnip could not locate an implementation for `tabular_auto_int` regression model
##   specifications using the `brulee` engine.
## i The parsnip extension package tabby implements support for this specification.
## i Please install (if needed) and load to continue.
```

```
## tabular auto int Model Specification (regression)
## 
## Main Arguments:
##   epochs = integer(1)
##   num_embedding = integer(1)
##   num_attn_feat = integer(1)
##   num_attn_heads = integer(1)
##   num_attn_blocks = integer(1)
##   activation = character(1)
##   dropout = double(1)
##   dropout_attn = double(1)
##   dropout_embedding = double(1)
##   penalty = double(1)
##   mixture = double(1)
##   learn_rate = double(1)
##   rate_schedule = tune()
##   momentum = character(1)
##   stop_iter = integer(1)
## 
## Computational engine: brulee
```

Note that parsnip automatically sets linear activation in the last layer. 

## Translation from parsnip to the original package (classification)


``` r
tabular_auto_int(
  epochs = integer(1),
  num_embedding = integer(1),
  hidden_units = NULL,
  hidden_activations = NULL,
  num_attn_feat = integer(1),
  num_attn_heads = integer(1),
  num_attn_blocks = integer(1),
  activation = character(1),
  dropout = double(1),
  dropout_attn = double(1),
  dropout_embedding = double(1),
  penalty = double(1),
  mixture = double(1),
  learn_rate = double(1),
  rate_schedule = tune(),
  momentum = character(1),
  batch_size = NULL,
  class_weights = NULL,
  stop_iter = integer(1)
) |> 
  set_engine("brulee") |> 
  set_mode("classification") |> 
  translate()
```

```
## ! parsnip could not locate an implementation for `tabular_auto_int` classification
##   model specifications using the `brulee` engine.
## i The parsnip extension package tabby implements support for this specification.
## i Please install (if needed) and load to continue.
```

```
## tabular auto int Model Specification (classification)
## 
## Main Arguments:
##   epochs = integer(1)
##   num_embedding = integer(1)
##   num_attn_feat = integer(1)
##   num_attn_heads = integer(1)
##   num_attn_blocks = integer(1)
##   activation = character(1)
##   dropout = double(1)
##   dropout_attn = double(1)
##   dropout_embedding = double(1)
##   penalty = double(1)
##   mixture = double(1)
##   learn_rate = double(1)
##   rate_schedule = tune()
##   momentum = character(1)
##   stop_iter = integer(1)
## 
## Computational engine: brulee
```

## Preprocessing requirements

`brulee_auto_int()` natively handles factor predictors via learned embeddings. Factor columns are automatically detected and embedded, while numeric columns use a scaled embedding. There is _no need to pre-encode factors as indicators_.


Predictors should have the same scale. One way to achieve this is to center and 
scale each so that each predictor has mean zero and a variance of one.

## Case weights


The underlying model implementation does not allow for case weights. 

## Prediction types


``` r
parsnip:::get_from_env("tabular_auto_int_predict") |>
  dplyr::filter(engine == "brulee") |>
  dplyr::select(mode, type)
```

```
## # A tibble: 0 x 2
## # i 2 variables: mode <chr>, type <chr>
```

## References

 - Song, W., Shi, C., Xiao, Z., Duan, Z., Xu, Y., Zhang, M., & Tang, J. (2019). AutoInt: Automatic Feature Interaction Learning via Self-Attentive Neural Networks. In _Proceedings of the 28th ACM International Conference on Information and Knowledge Management (CIKM)_.



