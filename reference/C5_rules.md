# C5.0 rule-based classification models

`C5_rules()` defines a model that derives feature rules from a tree for
prediction. A single tree or boosted ensemble can be used. This function
can fit classification models.

There are different ways to fit this model, and the method of estimation
is chosen by setting the model *engine*. The engine-specific pages for
this model are listed below.

- [`C5.0`](https://parsnip.tidymodels.org/reference/details_C5_rules_C5.0.md)`¹²`

¹ The default engine. ² Requires a parsnip extension package.

More information on how parsnip is used for modeling is at
<https://www.tidymodels.org/>.

## Usage

``` r
C5_rules(mode = "classification", trees = NULL, min_n = NULL, engine = "C5.0")
```

## Arguments

- mode:

  A single character string for the type of model. The only possible
  value for this model is "classification".

- trees:

  A non-negative integer (no greater than 100) for the number of members
  of the ensemble.

- min_n:

  An integer greater between zero and nine for the minimum number of
  data points in a node that are required for the node to be split
  further.

- engine:

  A single character string specifying what computational engine to use
  for fitting.

## Details

C5.0 is a classification model that is an extension of the C4.5 model of
Quinlan (1993). It has tree- and rule-based versions that also include
boosting capabilities. `C5_rules()` enables the version of the model
that uses a series of rules (see the examples below). To make a set of
rules, an initial C5.0 tree is created and flattened into rules. The
rules are pruned, simplified, and ordered. Rule sets are created within
each iteration of boosting.

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
    C5_rules(argument = !!value)

## References

Quinlan R (1993). *C4.5: Programs for Machine Learning*. Morgan Kaufmann
Publishers.

<https://www.tidymodels.org>, [*Tidy Modeling with
R*](https://www.tmwr.org/), [searchable table of parsnip
models](https://www.tidymodels.org/find/parsnip/)

## See also

[`C50::C5.0()`](https://topepo.github.io/C5.0/reference/C5.0.html),
[`C50::C5.0Control()`](https://topepo.github.io/C5.0/reference/C5.0Control.html),
[`fit()`](https://parsnip.tidymodels.org/reference/fit.md),
[`set_engine()`](https://parsnip.tidymodels.org/reference/set_engine.md),
[`update()`](https://rdrr.io/r/stats/update.html),
[`C5.0 engine details`](https://parsnip.tidymodels.org/reference/details_C5_rules_C5.0.md)

## Examples

``` r
show_engines("C5_rules")
#> # A tibble: 0 × 2
#> # ℹ 2 variables: engine <chr>, mode <chr>

C5_rules()
#> ! parsnip could not locate an implementation for `C5_rules` model
#>   specifications.
#> ℹ The parsnip extension package rules implements support for this
#>   specification.
#> ℹ Please install (if needed) and load to continue.
#> C5.0 Model Specification (classification)
#> 
#> Computational engine: C5.0 
#> 
```
