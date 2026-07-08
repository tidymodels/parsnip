# TabICL: prior data fitted networks

This function uses a pre-trained deep learning network that emulates
Bayesian inference. The model was trained on a large number of simulated
data sets and an attention mechanism is use to make relevant predictions
for specific (i.e., real) data sets.

There are different ways to fit this model, and the method of estimation
is chosen by setting the model *engine*. The engine-specific pages for
this model are listed below.

- [`brulee`](https://parsnip.tidymodels.org/dev/reference/details_tabular_icl_brulee.md)`¹²`

¹ The default engine. ² Requires a parsnip extension package for
classification and regression.

More information on how parsnip is used for modeling is at
<https://www.tidymodels.org/>.

## Usage

``` r
tabular_icl(
  mode = "unknown",
  engine = "brulee",
  num_estimators = NULL,
  softmax_temperature = NULL
)
```

## Arguments

- mode:

  A single character value for the type of model. The possible values
  for this model are "classification" and "regression".

- engine:

  A single character string specifying what computational engine to use
  for fitting. Possible engines are listed below. The default for this
  model is `"brulee"`.

- num_estimators:

  An integer for the ensemble size. Default is `8L`.

- softmax_temperature:

  An adjustment factor that is a divisor in the exponents of the softmax
  function (see `brulee::brulee_tab_icl()`). Defaults to 0.9.

## Details

This function fits classification and regression models.

## References

<https://github.com/soda-inria/tabicl>

Qu, J., Holzmüller, D., Varoquaux, G., & Morvan, M. L. (2025). Tabicl: A
tabular foundation model for in-context learning on large data. arXiv
preprint arXiv:2502.05564.

Qu, J., Holzmüller, D., Varoquaux, G., & Morvan, M. L. (2026). TabICLv2:
A better, faster, scalable, and open tabular foundation model. arXiv
preprint arXiv:2602.11139.

## See also

[`fit()`](https://parsnip.tidymodels.org/dev/reference/fit.md),
[`set_engine()`](https://parsnip.tidymodels.org/dev/reference/set_engine.md),
[`update()`](https://rdrr.io/r/stats/update.html),
[`brulee engine details`](https://parsnip.tidymodels.org/dev/reference/details_tabular_icl_brulee.md)
`brulee::brulee_tab_icl()`

## Examples

``` r
show_engines("tabular_icl")
#> # A tibble: 0 × 2
#> # ℹ 2 variables: engine <chr>, mode <chr>

tabular_icl()
#> ! parsnip could not locate an implementation for `tabular_icl` model
#>   specifications.
#> ℹ The parsnip extension package tabby implements support for this
#>   specification.
#> ℹ Please install (if needed) and load to continue.
#> tabular icl Model Specification (unknown mode)
#> 
#> Computational engine: brulee 
#> 
```
