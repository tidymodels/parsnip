# Extract survival time

Extract the time component(s) from a
[`survival::Surv()`](https://rdrr.io/pkg/survival/man/Surv.html) object.

## Arguments

- surv:

  A single
  [`survival::Surv()`](https://rdrr.io/pkg/survival/man/Surv.html)
  object.

## Value

A vector when the type is `"right"` or `"left"` and a tibble otherwise.
