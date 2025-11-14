# Resolve a Model Specification for a Computational Engine

`translate()` will translate a [model
specification](https://parsnip.tidymodels.org/dev/reference/model_spec.md)
into a code object that is specific to a particular engine (e.g. R
package). It translates generic parameters to their counterparts.

## Usage

``` r
translate(x, ...)

# Default S3 method
translate(x, engine = x$engine, ...)
```

## Arguments

- x:

  A [model
  specification](https://parsnip.tidymodels.org/dev/reference/model_spec.md).

- ...:

  Not currently used.

- engine:

  The computational engine for the model (see
  [`?set_engine`](https://parsnip.tidymodels.org/dev/reference/set_engine.md)).

## Details

`translate()` produces a *template* call that lacks the specific
argument values (such as `data`, etc). These are filled in once
[`fit()`](https://generics.r-lib.org/reference/fit.html) is called with
the specifics of the data for the model. The call may also include
[`tune()`](https://hardhat.tidymodels.org/reference/tune.html) arguments
if these are in the specification. To handle the
[`tune()`](https://hardhat.tidymodels.org/reference/tune.html)
arguments, you need to use the [tune
package](https://tune.tidymodels.org/). For more information see
<https://www.tidymodels.org/start/tuning/>

It does contain the resolved argument names that are specific to the
model fitting function/engine.

This function can be useful when you need to understand how parsnip goes
from a generic model specific to a model fitting function.

**Note**: this function is used internally and users should only use it
to understand what the underlying syntax would be. It should not be used
to modify the model specification.

## Examples

``` r
lm_spec <- linear_reg(penalty = 0.01)

# `penalty` is translated to `lambda`
translate(lm_spec, engine = "glmnet")
#> Linear Regression Model Specification (regression)
#> 
#> Main Arguments:
#>   penalty = 0.01
#> 
#> Computational engine: glmnet 
#> 
#> Model fit template:
#> glmnet::glmnet(x = missing_arg(), y = missing_arg(), weights = missing_arg(), 
#>     family = "gaussian")

# `penalty` not applicable for this model.
translate(lm_spec, engine = "lm")
#> Linear Regression Model Specification (regression)
#> 
#> Main Arguments:
#>   penalty = 0.01
#> 
#> Computational engine: lm 
#> 
#> Model fit template:
#> stats::lm(formula = missing_arg(), data = missing_arg(), weights = missing_arg())

# `penalty` is translated to `reg_param`
translate(lm_spec, engine = "spark")
#> Linear Regression Model Specification (regression)
#> 
#> Main Arguments:
#>   penalty = 0.01
#> 
#> Computational engine: spark 
#> 
#> Model fit template:
#> sparklyr::ml_linear_regression(x = missing_arg(), formula = missing_arg(), 
#>     weights = missing_arg(), reg_param = 0.01)

# with a placeholder for an unknown argument value:
translate(linear_reg(penalty = tune(), mixture = tune()), engine = "glmnet")
#> Linear Regression Model Specification (regression)
#> 
#> Main Arguments:
#>   penalty = tune()
#>   mixture = tune()
#> 
#> Computational engine: glmnet 
#> 
#> Model fit template:
#> glmnet::glmnet(x = missing_arg(), y = missing_arg(), weights = missing_arg(), 
#>     alpha = tune(), family = "gaussian")
```
