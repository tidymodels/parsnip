


For this engine, there are multiple modes: classification and regression

## Tuning Parameters



This model has 3 tuning parameters:

- `num_terms`: # Model Terms (type: integer, default: see below)

- `prod_degree`: Degree of Interaction (type: integer, default: 1L)

- `prune_method`: Pruning Method (type: character, default: 'backward')

Parsnip changes the default range for `num_terms` to `c(50, 500)`.

## Translation from parsnip to the original package (regression)


``` r
mars(num_terms = integer(1), prod_degree = integer(1), prune_method = character(1)) |> 
  set_engine("earth") |> 
  set_mode("regression") |> 
  translate()
```

```
## MARS Model Specification (regression)
## 
## Main Arguments:
##   num_terms = integer(1)
##   prod_degree = integer(1)
##   prune_method = character(1)
## 
## Computational engine: earth 
## 
## Model fit template:
## earth::earth(formula = missing_arg(), data = missing_arg(), weights = missing_arg(), 
##     nprune = integer(1), degree = integer(1), pmethod = character(1), 
##     keepxy = TRUE)
```

## Translation from parsnip to the original package (classification)


``` r
mars(num_terms = integer(1), prod_degree = integer(1), prune_method = character(1)) |> 
  set_engine("earth") |> 
  set_mode("classification") |> 
  translate()
```

```
## MARS Model Specification (classification)
## 
## Main Arguments:
##   num_terms = integer(1)
##   prod_degree = integer(1)
##   prune_method = character(1)
## 
## Engine-Specific Arguments:
##   glm = list(family = stats::binomial)
## 
## Computational engine: earth 
## 
## Model fit template:
## earth::earth(formula = missing_arg(), data = missing_arg(), weights = missing_arg(), 
##     nprune = integer(1), degree = integer(1), pmethod = character(1), 
##     glm = list(family = stats::binomial), keepxy = TRUE)
```

An alternate method for using MARs for categorical outcomes can be found in [discrim_flexible()].


## Preprocessing requirements


Factor/categorical predictors need to be converted to numeric values (e.g., dummy or indicator variables) for this engine. When using the formula method via \\code{\\link[=fit.model_spec]{fit()}}, parsnip will convert factor columns to indicators.

## Case weights


This model can utilize case weights during model fitting. To use them, see the documentation in [case_weights] and the examples on `tidymodels.org`. 

The `fit()` and `fit_xy()` arguments have arguments called `case_weights` that expect vectors of case weights. 

Note that the `earth` package documentation has: "In the current implementation, _building models with weights can be slow_."

## Saving fitted model objects


This model object contains data that are not required to make predictions. When saving the model for the purpose of prediction, the size of the saved object might be substantially reduced by using functions from the [butcher](https://butcher.tidymodels.org) package.

## Examples 

The "Fitting and Predicting with parsnip" article contains [examples](https://parsnip.tidymodels.org/articles/articles/Examples.html#mars-earth) for `mars()` with the `"earth"` engine.

## References

 - Friedman, J. 1991. "Multivariate Adaptive Regression Splines." _The Annals of Statistics_, vol. 19, no. 1, pp. 1-67.
 
 - Milborrow, S. ["Notes on the earth package."](http://www.milbo.org/doc/earth-notes.pdf) 
 
 - Kuhn, M, and K Johnson. 2013. _Applied Predictive Modeling_. Springer.

