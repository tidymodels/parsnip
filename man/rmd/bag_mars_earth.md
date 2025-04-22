


For this engine, there are multiple modes: classification and regression

## Tuning Parameters



This model has 3 tuning parameters:

- `prod_degree`: Degree of Interaction (type: integer, default: 1L)

- `prune_method`: Pruning Method (type: character, default: 'backward')

- `num_terms`: # Model Terms (type: integer, default: see below)

The default value of `num_terms` depends on the number of predictor columns. For a data frame `x`, the default is `min(200, max(20, 2 * ncol(x))) + 1` (see [earth::earth()] and the reference below). 

## Translation from parsnip to the original package (regression)

The **baguette** extension package is required to fit this model.


``` r
bag_mars(num_terms = integer(1), prod_degree = integer(1), prune_method = character(1)) |> 
  set_engine("earth") |> 
  set_mode("regression") |> 
  translate()
```

```
## Bagged MARS Model Specification (regression)
## 
## Main Arguments:
##   num_terms = integer(1)
##   prod_degree = integer(1)
##   prune_method = character(1)
## 
## Computational engine: earth 
## 
## Model fit template:
## baguette::bagger(formula = missing_arg(), data = missing_arg(), 
##     weights = missing_arg(), nprune = integer(1), degree = integer(1), 
##     pmethod = character(1), base_model = "MARS")
```

## Translation from parsnip to the original package (classification)

The **baguette** extension package is required to fit this model.


``` r
library(baguette)

bag_mars(
  num_terms = integer(1),
  prod_degree = integer(1),
  prune_method = character(1)
) |> 
  set_engine("earth") |> 
  set_mode("classification") |> 
  translate()
```

```
## Bagged MARS Model Specification (classification)
## 
## Main Arguments:
##   num_terms = integer(1)
##   prod_degree = integer(1)
##   prune_method = character(1)
## 
## Computational engine: earth 
## 
## Model fit template:
## baguette::bagger(formula = missing_arg(), data = missing_arg(), 
##     weights = missing_arg(), nprune = integer(1), degree = integer(1), 
##     pmethod = character(1), base_model = "MARS")
```

## Preprocessing requirements


Factor/categorical predictors need to be converted to numeric values (e.g., dummy or indicator variables) for this engine. When using the formula method via \\code{\\link[=fit.model_spec]{fit()}}, parsnip will convert factor columns to indicators.

## Case weights


This model can utilize case weights during model fitting. To use them, see the documentation in [case_weights] and the examples on `tidymodels.org`. 

The `fit()` and `fit_xy()` arguments have arguments called `case_weights` that expect vectors of case weights. 

Note that the `earth` package documentation has: "In the current implementation, _building models with weights can be slow_."

## References

 - Breiman, L. 1996. "Bagging predictors". Machine Learning. 24 (2): 123-140
 
 - Friedman, J. 1991. "Multivariate Adaptive Regression Splines." _The Annals of Statistics_, vol. 19, no. 1, pp. 1-67.
 
 - Milborrow, S. ["Notes on the earth package."](http://www.milbo.org/doc/earth-notes.pdf) 
 
 - Kuhn, M, and K Johnson. 2013. _Applied Predictive Modeling_. Springer.

