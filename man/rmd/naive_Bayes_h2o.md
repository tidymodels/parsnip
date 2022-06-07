


For this engine, there is a single mode: classification

## Tuning Parameters




This model has 1 tuning parameter:

- `Laplace`: Laplace Correction (type: double, default: 0.0)

[h2o::h2o.naiveBayes()] provides several engine arguments to deal with imbalances and rare classes: 

- `balance_classes` A logical value controlling over/under-sampling (for imbalanced data). Defaults to `FALSE`.

- `class_sampling_factors` The over/under-sampling ratios per class (in lexicographic order). If not specified, sampling factors will be automatically computed to obtain class balance during training. Require `balance_classes` to be `TRUE`.

- `min_sdev`: The minimum standard deviation to use for observations without enough data, must be greater than 1e-10.

- `min_prob`: The minimum probability to use for observations with not enough data.


## Translation from parsnip to the original package



[agua::h2o_train_nb()] is a wrapper around [h2o.naiveBayes()]. 


```r
naive_Bayes(Laplace = numeric(0)) %>% 
  set_engine("h2o") %>% 
  translate()
```

```
## Naive Bayes Model Specification (classification)
## 
## Main Arguments:
##   Laplace = numeric(0)
## 
## Computational engine: h2o 
## 
## Model fit template:
## agua::h2o_train_nb(x = missing_arg(), y = missing_arg(), laplace = numeric(0))
```
