


For this engine, there are multiple modes: classification and regression

## Tuning Parameters



This model has 4 tuning parameters:

- `trees`: # Trees (type: integer, default: 200L)

- `prior_terminal_node_coef`: Terminal Node Prior Coefficient (type: double, default: 0.95)

- `prior_terminal_node_expo`: Terminal Node Prior Exponent (type: double, default: 2.00)

- `prior_outcome_range`: Prior for Outcome Range (type: double, default: 2.00)

Parsnip changes the default range for `trees` to `c(50, 500)`.

## Important engine-specific options

Some relevant arguments that can be passed to `set_engine()`: 

* `keepevery`, `n.thin`:	Every `keepevery` draw is kept to be returned to the user. Useful for "thinning" samples.

* `ntree`, `n.trees`: The number of trees in the sum-of-trees formulation.

* `ndpost`, `n.samples`: The number of posterior draws after burn in, `ndpost` / `keepevery` will actually be returned.

* `nskip`, `n.burn`: Number of MCMC iterations to be treated as burn in.

* `nchain`, `n.chains`: Integer specifying how many independent tree sets and fits should be calculated.

* `nthread`, `n.threads`: Integer specifying how many threads to use. Depending on the CPU architecture, using more than the number of chains can degrade performance for small/medium data sets. As such some calculations may be executed single threaded regardless.

* `combinechains`, `combineChains`: Logical; if `TRUE`, samples will be returned in arrays of dimensions equal to `nchain` times `ndpost` times number of observations.

## Translation from parsnip to the original package (classification)


``` r
parsnip::bart(
  trees = integer(1),
  prior_terminal_node_coef = double(1),
  prior_terminal_node_expo = double(1),
  prior_outcome_range = double(1)
) |> 
  set_engine("dbarts") |> 
  set_mode("classification") |> 
  translate() |> 
  print_model_spec()
```

```
## BART Model Specification (classification)
## 
## Main Arguments:
##   trees = integer(1)
##   prior_terminal_node_coef = double(1)
##   prior_terminal_node_expo = double(1)
##   prior_outcome_range = double(1)
## 
## Computational engine: dbarts 
## 
## Model fit template:
## dbarts::bart(x = missing_arg(), y = missing_arg(), ntree = integer(1), 
##     base = double(1), power = double(1), k = double(1), verbose = FALSE, 
##     keeptrees = TRUE, keepcall = FALSE)
```


## Translation from parsnip to the original package (regression)


``` r
parsnip::bart(
  trees = integer(1),
  prior_terminal_node_coef = double(1),
  prior_terminal_node_expo = double(1),
  prior_outcome_range = double(1)
) |> 
  set_engine("dbarts") |> 
  set_mode("regression") |> 
  translate()|> 
  print_model_spec()
```

```
## BART Model Specification (regression)
## 
## Main Arguments:
##   trees = integer(1)
##   prior_terminal_node_coef = double(1)
##   prior_terminal_node_expo = double(1)
##   prior_outcome_range = double(1)
## 
## Computational engine: dbarts 
## 
## Model fit template:
## dbarts::bart(x = missing_arg(), y = missing_arg(), ntree = integer(1), 
##     base = double(1), power = double(1), k = double(1), verbose = FALSE, 
##     keeptrees = TRUE, keepcall = FALSE)
```

## Preprocessing requirements


Factor/categorical predictors need to be converted to numeric values (e.g., dummy or indicator variables) for this engine. When using the formula method via \\code{\\link[=fit.model_spec]{fit()}}, parsnip will convert factor columns to indicators.

[dbarts::bart()] will also convert the factors to indicators if the user does not create them first. 


## References

 - Chipman, George, McCulloch. "BART: Bayesian additive regression trees." _Ann. Appl. Stat._ 4 (1) 266 - 298, March 2010.
