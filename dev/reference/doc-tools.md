# Tools for documenting engines

parsnip has a fairly complex documentation system where the engines for
each model have detailed documentation about the syntax, tuning
parameters, preprocessing needs, and so on.

The functions below are called from `.R` files to programmatically
generate content in the help files for a model.

- `find_engine_files()` identifies engines for a model and creates a
  bulleted list of links to those specific help files.

- `make_seealso_list()` creates a set of links for the "See Also" list
  at the bottom of the help pages.

- `find_engine_files()` is a function, used by the above, to find the
  engines for each model function.

## Usage

``` r
find_engine_files(mod)

make_engine_list(mod)

make_seealso_list(mod, pkg = "parsnip")
```

## Arguments

- mod:

  A character string for the model file (e.g. "linear_reg")

- pkg:

  A character string for the package where the function is invoked.

## Value

`make_engine_list()` returns a character string that creates a bulleted
list of links to more specific help files.

`make_seealso_list()` returns a formatted character string of links.

`find_engine_files()` returns a tibble.

## Details

parsnip includes a document (`README-DOCS.md`) with step-by-step
instructions and details. See the code below to determine where it is
installed (or see the References section).

Most parsnip users will not need to use these functions or
documentation.

## References

<https://github.com/tidymodels/parsnip/blob/main/inst/README-DOCS.md>

## Examples

``` r
# See this file for step-by-step instructions.
system.file("README-DOCS.md", package = "parsnip")
#> [1] "/home/runner/work/_temp/Library/parsnip/README-DOCS.md"

# Code examples:
make_engine_list("linear_reg")
#> There are different ways to fit this model, and the method of estimation is  chosen by setting the model \emph{engine}. The engine-specific pages  for this model are listed  below.
#> 
#> 
#> \itemize{
#>   \item \code{\link[parsnip:details_linear_reg_lm]{lm}¹}
#>   \item \code{\link[parsnip:details_linear_reg_brulee]{brulee}}
#>   \item \code{\link[parsnip:details_linear_reg_gee]{gee}²}
#>   \item \code{\link[parsnip:details_linear_reg_glm]{glm}}
#>   \item \code{\link[parsnip:details_linear_reg_glmer]{glmer}²}
#>   \item \code{\link[parsnip:details_linear_reg_glmnet]{glmnet}}
#>   \item \code{\link[parsnip:details_linear_reg_gls]{gls}²}
#>   \item \code{\link[parsnip:details_linear_reg_h2o]{h2o}²}
#>   \item \code{\link[parsnip:details_linear_reg_keras]{keras}}
#>   \item \code{\link[parsnip:details_linear_reg_lme]{lme}²}
#>   \item \code{\link[parsnip:details_linear_reg_lmer]{lmer}²}
#>   \item \code{\link[parsnip:details_linear_reg_quantreg]{quantreg}}
#>   \item \code{\link[parsnip:details_linear_reg_spark]{spark}}
#>   \item \code{\link[parsnip:details_linear_reg_stan]{stan}}
#>   \item \code{\link[parsnip:details_linear_reg_stan_glmer]{stan_glmer}²}
#> }
#> 
#>  
#> ¹ The default engine. ² Requires a parsnip extension package for regression.

cat(make_engine_list("linear_reg"))
#> There are different ways to fit this model, and the method of estimation is  chosen by setting the model \emph{engine}. The engine-specific pages  for this model are listed  below.
#> 
#> 
#> \itemize{
#>   \item \code{\link[parsnip:details_linear_reg_lm]{lm}¹}
#>   \item \code{\link[parsnip:details_linear_reg_brulee]{brulee}}
#>   \item \code{\link[parsnip:details_linear_reg_gee]{gee}²}
#>   \item \code{\link[parsnip:details_linear_reg_glm]{glm}}
#>   \item \code{\link[parsnip:details_linear_reg_glmer]{glmer}²}
#>   \item \code{\link[parsnip:details_linear_reg_glmnet]{glmnet}}
#>   \item \code{\link[parsnip:details_linear_reg_gls]{gls}²}
#>   \item \code{\link[parsnip:details_linear_reg_h2o]{h2o}²}
#>   \item \code{\link[parsnip:details_linear_reg_keras]{keras}}
#>   \item \code{\link[parsnip:details_linear_reg_lme]{lme}²}
#>   \item \code{\link[parsnip:details_linear_reg_lmer]{lmer}²}
#>   \item \code{\link[parsnip:details_linear_reg_quantreg]{quantreg}}
#>   \item \code{\link[parsnip:details_linear_reg_spark]{spark}}
#>   \item \code{\link[parsnip:details_linear_reg_stan]{stan}}
#>   \item \code{\link[parsnip:details_linear_reg_stan_glmer]{stan_glmer}²}
#> }
#> 
#>  
#> ¹ The default engine. ² Requires a parsnip extension package for regression.
```
