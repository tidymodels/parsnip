


For this engine, there is a single mode: classification

## Tuning Parameters



This model has 0 tuning parameters:



## Translation from parsnip to the original package

The **ordered** extension package is required to fit this model.


``` r
library(ordered)

ordinal_reg() %>% 
  set_engine("vglm") %>% 
  set_mode("classification") %>% 
  translate()
```

```
## Ordinal Regression Model Specification (classification)
## 
## Computational engine: vglm 
## 
## Model fit template:
## ordered::VGAM_vglm_wrapper(formula = missing_arg(), data = missing_arg(), 
##     weights = missing_arg(), parallel = TRUE)
```

## References

- Yee T. 2010. The VGAM Package for Categorical Data Analysis. _Journal of Statistical Software_ 32(10):1--34. \doi{10.18637/jss.v032.i10}.
