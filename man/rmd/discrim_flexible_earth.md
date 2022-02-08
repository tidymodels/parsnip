


For this engine, there is a single mode: classification

## Tuning Parameters



This model has 3 tuning parameter:

- `num_terms`: # Model Terms (type: integer, default: (see below))

- `prod_degree`: Degree of Interaction (type: integer, default: 1L)

- `prune_method`: Pruning Method (type: character, default: 'backward')

The default value of `num_terms` depends on the number of columns (`p`): `min(200, max(20, 2 * p)) + 1`. Note that `num_terms = 1` is an intercept-only model. 

## Translation from parsnip to the original package

There is a parsnip extension package required to fit this model to this mode: **discrim**.


```r
library(discrim)

discrim_flexible(
  num_terms = integer(0),
  prod_degree = integer(0),
  prune_method = character(0)
) %>% 
  translate()
```

```
## Flexible Discriminant Model Specification (classification)
## 
## Main Arguments:
##   num_terms = integer(0)
##   prod_degree = integer(0)
##   prune_method = character(0)
## 
## Computational engine: earth 
## 
## Model fit template:
## mda::fda(formula = missing_arg(), data = missing_arg(), nprune = integer(0), 
##     degree = integer(0), pmethod = character(0), method = earth::earth)
```

## Preprocessing requirements


Factor/categorical predictors need to be converted to numeric values (e.g., dummy or indicator variables) for this engine. When using the formula method via \\code{\\link[=fit.model_spec]{fit.model_spec()}}, parsnip will convert factor columns to indicators.


## References

 - Hastie, Tibshirani & Buja (1994) Flexible Discriminant Analysis by Optimal 
   Scoring, _Journal of the American Statistical Association_, 89:428, 1255-1270
   
 - Friedman (1991). Multivariate Adaptive Regression Splines. _The Annals of Statistics_, 19(1), 1-67.
