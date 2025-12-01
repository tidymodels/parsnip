# Determine if case weights are used

Not all modeling engines can incorporate case weights into their
calculations. This function can determine whether they can be used.

## Usage

``` r
case_weights_allowed(spec)
```

## Arguments

- spec:

  A parsnip [model
  specification](https://parsnip.tidymodels.org/reference/model_spec.md).

## Value

A single logical.

## Examples

``` r
case_weights_allowed(linear_reg())
#> [1] TRUE
case_weights_allowed(linear_reg(engine = "keras"))
#> [1] FALSE
```
