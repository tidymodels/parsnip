# TabPFN: prior data fitted networks

This function uses a pre-trained deep learning network that emulates
Bayesian inference. The model was trained on a large number of simulated
data sets and an attention mechanism is use to make relevant predictions
for specific (i.e., real) data sets.

There are different ways to fit this model, and the method of estimation
is chosen by setting the model *engine*. The engine-specific pages for
this model are listed below.

- [`tabpfn`](https://parsnip.tidymodels.org/dev/reference/details_tabular_pfn_tabpfn.md)`¹²`

¹ The default engine. ² Requires a parsnip extension package for
classification and regression.

More information on how parsnip is used for modeling is at
<https://www.tidymodels.org/>.

## Usage

``` r
tabular_pfn(
  mode = "unknown",
  engine = "tabpfn",
  num_estimators = NULL,
  softmax_temperature = NULL,
  balance_probabilities = NULL,
  average_before_softmax = NULL
)
```

## Arguments

- mode:

  A single character value for the type of model. The possible values
  for this model are "classification" and "regression".

- engine:

  A single character string specifying what computational engine to use
  for fitting. Possible engines are listed below. The default for this
  model is `"tabpfn"`.

- num_estimators:

  An integer for the ensemble size. Default is `8L`.

- softmax_temperature:

  An adjustment factor that is a divisor in the exponents of the softmax
  function (see `tabpfn::tab_pfn()`). Defaults to 0.9.

- balance_probabilities:

  A logical to adjust the prior probabilities in cases where there is a
  class imbalance. Default is `FALSE`. Classification only.

- average_before_softmax:

  A logical. For cases where `num_estimators > 1`, should the average be
  done before using the softmax function or after? Default is `FALSE`.

## Details

This function fits classification and regression models.

## References

<https://github.com/PriorLabs/TabPFN>

Hollmann, Noah, Samuel Müller, Lennart Purucker, Arjun Krishnakumar, Max
Körfer, Shi Bin Hoo, Robin Tibor Schirrmeister, and Frank Hutter.
"Accurate predictions on small data with a tabular foundation model."
*Nature* 637, no. 8045 (2025): 319-326.

Hollmann, Noah, Samuel Müller, Katharina Eggensperger, and Frank Hutter.
"Tabpfn: A transformer that solves small tabular classification problems
in a second." *arXiv preprint* arXiv:2207.01848 (2022).

Müller, Samuel, Noah Hollmann, Sebastian Pineda Arango, Josif Grabocka,
and Frank Hutter. "Transformers can do Bayesian inference." *arXiv
preprint* arXiv:2112.10510 (2021).

## See also

[`fit()`](https://parsnip.tidymodels.org/dev/reference/fit.md),
[`set_engine()`](https://parsnip.tidymodels.org/dev/reference/set_engine.md),
[`update()`](https://rdrr.io/r/stats/update.html),
[`tabpfn engine details`](https://parsnip.tidymodels.org/dev/reference/details_tabular_pfn_tabpfn.md)
`tabpfn::tab_pfn()`

## Examples

``` r
show_engines("tabular_pfn")
#> # A tibble: 0 × 2
#> # ℹ 2 variables: engine <chr>, mode <chr>

tabular_pfn()
#> ! parsnip could not locate an implementation for `tabular_pfn` model
#>   specifications.
#> ℹ The parsnip extension package tabby implements support for this
#>   specification.
#> ℹ Please install (if needed) and load to continue.
#> tabular pfn Model Specification (unknown mode)
#> 
#> Computational engine: tabpfn 
#> 
```
