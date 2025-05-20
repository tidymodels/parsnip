


For this engine, there is a single mode: classification

## Tuning Parameters




This model has 2 tuning parameter:

- `smoothness`: Kernel Smoothness (type: double, default: 1.0)

- `Laplace`: Laplace Correction (type: double, default: 0.0)

Note that the engine argument `usekernel` is set to `TRUE` by default when using the `naivebayes` engine. 

## Translation from parsnip to the original package

The **discrim** extension package is required to fit this model.


``` r
library(discrim)

naive_Bayes(smoothness = numeric(0), Laplace = numeric(0)) |> 
  set_engine("naivebayes") |> 
  translate()
```

```
## Naive Bayes Model Specification (classification)
## 
## Main Arguments:
##   smoothness = numeric(0)
##   Laplace = numeric(0)
## 
## Computational engine: naivebayes 
## 
## Model fit template:
## naivebayes::naive_bayes(x = missing_arg(), y = missing_arg(), 
##     adjust = numeric(0), laplace = numeric(0), usekernel = TRUE)
```

## Preprocessing requirements

The columns for qualitative predictors should always be represented as factors (as opposed to dummy/indicator variables). When the predictors are factors, the underlying code treats them as multinomial data and appropriately computes their conditional distributions. 

For count data, integers can be estimated using a Poisson distribution if the argument `usepoisson = TRUE` is passed as an engine argument.  


Variance calculations are used in these computations so _zero-variance_ predictors (i.e., with a single unique value) should be eliminated before fitting the model. 



## Case weights


The underlying model implementation does not allow for case weights. 

## References

 - Kuhn, M, and K Johnson. 2013. _Applied Predictive Modeling_. Springer.
