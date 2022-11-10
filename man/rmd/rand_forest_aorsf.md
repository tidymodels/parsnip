


For this engine, there is a single mode: censored regression

## Tuning Parameters



This model has 3 tuning parameters:

- `trees`: # Trees (type: integer, default: 500L)

- `min_n`: Minimal Node Size (type: integer, default: 5L)

- `mtry`: # Randomly Selected Predictors (type: integer, default: ceiling(sqrt(n_predictors)))

Additionally, this model has one engine-specific tuning parameter:

 * `split_min_stat`: Minimum test statistic required to split a node. Default is `3.841459` for the log-rank test, which is roughly a p-value of 0.05.


# Translation from parsnip to the original package (censored regression)

The **censored** extension package is required to fit this model.


```r
library(censored)

rand_forest() %>%
  set_engine("aorsf") %>%
  set_mode("censored regression") %>%
  translate()
```

```
## Random Forest Model Specification (censored regression)
## 
## Computational engine: aorsf 
## 
## Model fit template:
## aorsf::orsf(formula = missing_arg(), data = missing_arg(), weights = missing_arg())
```

## Preprocessing requirements


This engine does not require any special encoding of the predictors. Categorical predictors can be partitioned into groups of factor levels (e.g. `{a, c}` vs `{b, d}`) when splitting at a node. Dummy variables are not required for this model. 

## Case weights


This model can utilize case weights during model fitting. To use them, see the documentation in [case_weights] and the examples on `tidymodels.org`. 

The `fit()` and `fit_xy()` arguments have arguments called `case_weights` that expect vectors of case weights. 

## Other details

Predictions of survival probability at a time exceeding the maximum observed event time are the predicted survival probability at the maximum observed time in the training data.

## References

- Jaeger BC, Long DL, Long DM, Sims M, Szychowski JM, Min YI, Mcclure LA, Howard G, Simon N. Oblique random survival forests. Annals of applied statistics 2019 Sep; 13(3):1847-83. DOI: 10.1214/19-AOAS1261

- Jaeger BC, Welden S, Lenoir K, Pajewski NM. aorsf: An R package for supervised learning using the oblique random survival forest. Journal of Open Source Software 2022, 7(77), 1 4705. \doi{10.21105/joss.04705}.

- Jaeger BC, Welden S, Lenoir K, Speiser JL, Segar MW, Pandey A, Pajewski NM. Accelerated and interpretable oblique random survival forests. arXiv e-prints 2022 Aug; arXiv-2208. URL: https://arxiv.org/abs/2208.01129
