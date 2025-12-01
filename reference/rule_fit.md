# RuleFit models

`rule_fit()` defines a model that derives simple feature rules from a
tree ensemble and uses them as features in a regularized model. This
function can fit classification and regression models.

There are different ways to fit this model, and the method of estimation
is chosen by setting the model *engine*. The engine-specific pages for
this model are listed below.

- [`xrf`](https://parsnip.tidymodels.org/reference/details_rule_fit_xrf.md)`¹²`

- [`h2o`](https://parsnip.tidymodels.org/reference/details_rule_fit_h2o.md)`²`

¹ The default engine. ² Requires a parsnip extension package for
classification and regression.

More information on how parsnip is used for modeling is at
<https://www.tidymodels.org/>.

## Usage

``` r
rule_fit(
  mode = "unknown",
  mtry = NULL,
  trees = NULL,
  min_n = NULL,
  tree_depth = NULL,
  learn_rate = NULL,
  loss_reduction = NULL,
  sample_size = NULL,
  stop_iter = NULL,
  penalty = NULL,
  engine = "xrf"
)
```

## Arguments

- mode:

  A single character string for the prediction outcome mode. Possible
  values for this model are "unknown", "regression", or
  "classification".

- mtry:

  A number for the number (or proportion) of predictors that will be
  randomly sampled at each split when creating the tree models (specific
  engines only).

- trees:

  An integer for the number of trees contained in the ensemble.

- min_n:

  An integer for the minimum number of data points in a node that is
  required for the node to be split further.

- tree_depth:

  An integer for the maximum depth of the tree (i.e. number of splits)
  (specific engines only).

- learn_rate:

  A number for the rate at which the boosting algorithm adapts from
  iteration-to-iteration (specific engines only). This is sometimes
  referred to as the shrinkage parameter.

- loss_reduction:

  A number for the reduction in the loss function required to split
  further (specific engines only).

- sample_size:

  A number for the number (or proportion) of data that is exposed to the
  fitting routine. For `xgboost`, the sampling is done at each iteration
  while `C5.0` samples once during training.

- stop_iter:

  The number of iterations without improvement before stopping (specific
  engines only).

- penalty:

  L1 regularization parameter.

- engine:

  A single character string specifying what computational engine to use
  for fitting.

## Details

The RuleFit model creates a regression model of rules in two stages. The
first stage uses a tree-based model that is used to generate a set of
rules that can be filtered, modified, and simplified. These rules are
then added as predictors to a regularized generalized linear model that
can also conduct feature selection during model training.

This function only defines what *type* of model is being fit. Once an
engine is specified, the *method* to fit the model is also defined. See
[`set_engine()`](https://parsnip.tidymodels.org/reference/set_engine.md)
for more on setting the engine, including how to set engine arguments.

The model is not trained or fit until the
[`fit()`](https://parsnip.tidymodels.org/reference/fit.md) function is
used with the data.

Each of the arguments in this function other than `mode` and `engine`
are captured as
[quosures](https://rlang.r-lib.org/reference/topic-quosure.html). To
pass values programmatically, use the [injection
operator](https://rlang.r-lib.org/reference/injection-operator.html)
like so:

    value <- 1
    rule_fit(argument = !!value)

## References

Friedman, J. H., and Popescu, B. E. (2008). "Predictive learning via
rule ensembles." *The Annals of Applied Statistics*, 2(3), 916-954.

<https://www.tidymodels.org>, [*Tidy Modeling with
R*](https://www.tmwr.org/), [searchable table of parsnip
models](https://www.tidymodels.org/find/parsnip/)

## See also

`xrf::xrf.formula()`,
[`fit()`](https://parsnip.tidymodels.org/reference/fit.md),
[`set_engine()`](https://parsnip.tidymodels.org/reference/set_engine.md),
[`update()`](https://rdrr.io/r/stats/update.html),
[`xrf engine details`](https://parsnip.tidymodels.org/reference/details_rule_fit_xrf.md),
[`h2o engine details`](https://parsnip.tidymodels.org/reference/details_rule_fit_h2o.md)

## Examples

``` r
show_engines("rule_fit")
#> # A tibble: 0 × 2
#> # ℹ 2 variables: engine <chr>, mode <chr>

rule_fit()
#> ! parsnip could not locate an implementation for `rule_fit` model
#>   specifications.
#> ℹ The parsnip extension packages agua and rules implement support for
#>   this specification.
#> ℹ Please install (if needed) and load to continue.
#> RuleFit Model Specification (unknown mode)
#> 
#> Computational engine: xrf 
#> 
```
