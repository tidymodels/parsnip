# Condense control object into strictly smaller control object

This function is used to help the hierarchy of control functions used
throughout the tidymodels packages. It is now assumed that each control
function is either a subset or a superset of another control function.

## Usage

``` r
condense_control(x, ref, ..., call = rlang::caller_env())
```

## Arguments

- x:

  A control object to be condensed.

- ref:

  A control object that is used to determine what element should be
  kept.

- call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the call argument of
  [`rlang::abort()`](https://rlang.r-lib.org/reference/abort.html) for
  more information.

## Value

A control object with the same elements and classes of `ref`, with
values of `x`.

## Examples

``` r
ctrl <- control_parsnip(catch = TRUE)
ctrl$allow_par <- TRUE
str(ctrl)
#> List of 3
#>  $ verbosity: int 1
#>  $ catch    : logi TRUE
#>  $ allow_par: logi TRUE
#>  - attr(*, "class")= chr "control_parsnip"

ctrl <- condense_control(ctrl, control_parsnip())
str(ctrl)
#> List of 2
#>  $ verbosity: int 1
#>  $ catch    : logi TRUE
#>  - attr(*, "class")= chr "control_parsnip"
```
