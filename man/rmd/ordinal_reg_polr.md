


For this engine, there is a single mode: classification

## Tuning Parameters

This model has no tuning parameters.

## Translation from parsnip to the original package

The **ordered** extension package is required to fit this model.


``` r
library(ordered)

ordinal_reg() %>% 
  set_engine("polr") %>% 
  set_mode("classification") %>% 
  translate()
```

```
## Ordinal Regression Model Specification (classification)
## 
## Computational engine: polr 
## 
## Model fit template:
## MASS::polr(formula = missing_arg(), data = missing_arg(), weights = missing_arg())
```

## References

- Bürkner P-C, Vuorre M. 2019. Ordinal Regression Models in Psychology: A Tutorial. _Advances in Methods and Practices in Psychological Science_ 2(1):77-101. \doi{10.1177/2515245918823199}.
