# Determine required packages for a model

Determine required packages for a model

## Usage

``` r
# S3 method for class 'model_spec'
required_pkgs(x, infra = TRUE, ...)

# S3 method for class 'model_fit'
required_pkgs(x, infra = TRUE, ...)
```

## Arguments

- x:

  A [model
  specification](https://parsnip.tidymodels.org/dev/reference/model_spec.md)
  or [fit](https://parsnip.tidymodels.org/dev/reference/model_fit.md).

- infra:

  Should parsnip itself be included in the result?

- ...:

  Not used.

## Value

A character vector

## Examples

``` r
should_fail <- try(required_pkgs(linear_reg(engine = NULL)), silent = TRUE)
should_fail
#> [1] "Error in required_pkgs(linear_reg(engine = NULL)) : \n  \033[1m\033[22mPlease set an engine.\n"
#> attr(,"class")
#> [1] "try-error"
#> attr(,"condition")
#> <error/rlang_error>
#> Error in `required_pkgs()`:
#> ! Please set an engine.
#> ---
#> Backtrace:
#>      ▆
#>   1. └─pkgdown::build_site_github_pages(new_process = FALSE, install = FALSE)
#>   2.   └─pkgdown::build_site(...)
#>   3.     └─pkgdown:::build_site_local(...)
#>   4.       └─pkgdown::build_reference(...)
#>   5.         ├─pkgdown:::unwrap_purrr_error(...)
#>   6.         │ └─base::withCallingHandlers(...)
#>   7.         └─purrr::map(...)
#>   8.           └─purrr:::map_("list", .x, .f, ..., .progress = .progress)
#>   9.             ├─purrr:::with_indexed_errors(...)
#>  10.             │ └─base::withCallingHandlers(...)
#>  11.             ├─purrr:::call_with_cleanup(...)
#>  12.             └─pkgdown (local) .f(.x[[i]], ...)
#>  13.               ├─base::withCallingHandlers(...)
#>  14.               └─pkgdown:::data_reference_topic(...)
#>  15.                 └─pkgdown:::run_examples(...)
#>  16.                   └─pkgdown:::highlight_examples(code, topic, env = env)
#>  17.                     └─downlit::evaluate_and_highlight(...)
#>  18.                       └─evaluate::evaluate(code, child_env(env), new_device = TRUE, output_handler = output_handler)
#>  19.                         ├─base::withRestarts(...)
#>  20.                         │ └─base (local) withRestartList(expr, restarts)
#>  21.                         │   ├─base (local) withOneRestart(withRestartList(expr, restarts[-nr]), restarts[[nr]])
#>  22.                         │   │ └─base (local) doWithOneRestart(return(expr), restart)
#>  23.                         │   └─base (local) withRestartList(expr, restarts[-nr])
#>  24.                         │     └─base (local) withOneRestart(expr, restarts[[1L]])
#>  25.                         │       └─base (local) doWithOneRestart(return(expr), restart)
#>  26.                         ├─evaluate:::with_handlers(...)
#>  27.                         │ ├─base::eval(call)
#>  28.                         │ │ └─base::eval(call)
#>  29.                         │ └─base::withCallingHandlers(...)
#>  30.                         ├─base::withVisible(eval(expr, envir))
#>  31.                         └─base::eval(expr, envir)
#>  32.                           └─base::eval(expr, envir)
#>  33.                             ├─base::try(required_pkgs(linear_reg(engine = NULL)), silent = TRUE)
#>  34.                             │ └─base::tryCatch(...)
#>  35.                             │   └─base (local) tryCatchList(expr, classes, parentenv, handlers)
#>  36.                             │     └─base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])
#>  37.                             │       └─base (local) doTryCatch(return(expr), name, parentenv, handler)
#>  38.                             ├─generics::required_pkgs(linear_reg(engine = NULL))
#>  39.                             └─parsnip:::required_pkgs.model_spec(linear_reg(engine = NULL))

linear_reg() |>
  set_engine("glmnet") |>
  required_pkgs()
#> [1] "parsnip" "glmnet" 

linear_reg() |>
  set_engine("glmnet") |>
  required_pkgs(infra = FALSE)
#> [1] "glmnet"

linear_reg() |>
  set_engine("lm") |>
  fit(mpg ~ ., data = mtcars) |>
  required_pkgs()
#> [1] "parsnip" "stats"  
```
