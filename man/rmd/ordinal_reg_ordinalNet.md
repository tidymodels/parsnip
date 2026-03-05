


For this engine, there is a single mode: classification

## Tuning Parameters



This model has 2 tuning parameters:

- `penalty`: Amount of Regularization (type: double, default: see below)

- `mixture`: Proportion of Lasso Penalty (type: double, default: 1.0)

## Translation from parsnip to the original package

The **ordered** extension package is required to fit this model.


``` r
library(ordered)

ordinal_reg() %>% 
  set_engine("ordinalNet") %>% 
  set_mode("classification") %>% 
  translate()
```

```
## Ordinal Regression Model Specification (classification)
## 
## Computational engine: ordinalNet 
## 
## Model fit template:
## ordered::ordinalNet_wrapper(x = missing_arg(), y = missing_arg(), 
##     weights = missing_arg(), nLambda = 120L, lambdaMinRatio = 1e-08, 
##     includeLambda0 = TRUE)
```

## References

- Wurm MJ, Rathouz PJ, Hanlon BM. 2021. Regularized Ordinal Regression and the ordinalNet R Package. _Journal of Statistical Software_ 99(6):1-42. \doi{10.18637/jss.v099.i06}.
