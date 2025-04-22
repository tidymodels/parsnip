#' Formulas with special terms in tidymodels
#'
#' @description
#'
#' In R, formulas provide a compact, symbolic notation to specify model terms.
#' Many modeling functions in R make use of ["specials"][stats::terms.formula],
#' or nonstandard notations used in formulas. Specials are defined and handled as
#' a special case by a given modeling package. For example, the mgcv package,
#' which provides support for
#' [generalized additive models][parsnip::gen_additive_mod] in R, defines a
#' function `s()` to be in-lined into formulas. It can be used like so:
#'
#' ``` r
#' mgcv::gam(mpg ~ wt + s(disp, k = 5), data = mtcars)
#' ```
#'
#' In this example, the `s()` special defines a smoothing term that the mgcv
#' package knows to look for when preprocessing model input.
#'
#' The parsnip package can handle most specials without issue. The analogous
#' code for specifying this generalized additive model
#' [with the parsnip "mgcv" engine][parsnip::details_gen_additive_mod_mgcv]
#' looks like:
#'
#' ``` r
#' gen_additive_mod() |>
#'   set_mode("regression") |>
#'   set_engine("mgcv") |>
#'   fit(mpg ~ wt + s(disp, k = 5), data = mtcars)
#' ```
#'
#' However, parsnip is often used in conjunction with the greater tidymodels
#' package ecosystem, which defines its own pre-processing infrastructure and
#' functionality via packages like hardhat and recipes. The specials defined
#' in many modeling packages introduce conflicts with that infrastructure.
#'
#' To support specials while also maintaining consistent syntax elsewhere in
#' the ecosystem, **tidymodels delineates between two types of formulas:
#' preprocessing formulas and model formulas**. Preprocessing formulas specify
#' the input variables, while model formulas determine the model structure.
#'
#' @section Example:
#'
#' To create the preprocessing formula from the model formula, just remove
#' the specials, retaining references to input variables themselves. For example:
#'
#' ```
#' model_formula <- mpg ~ wt + s(disp, k = 5)
#' preproc_formula <- mpg ~ wt + disp
#' ```
#'
#' \itemize{
#'   \item **With parsnip,** use the model formula:
#'
#'    ``` r
#'    model_spec <-
#'      gen_additive_mod() |>
#'      set_mode("regression") |>
#'      set_engine("mgcv")
#'
#'    model_spec |>
#'      fit(model_formula, data = mtcars)
#'    ```
#'
#'    \item **With recipes**, use the preprocessing formula only:
#'
#'    ``` r
#'    library(recipes)
#'
#'    recipe(preproc_formula, mtcars)
#'    ```
#'
#'    The recipes package supplies a large variety of preprocessing techniques
#'    that may replace the need for specials altogether, in some cases.
#'
#'   \item **With workflows,** use the preprocessing formula everywhere, but
#'   pass the model formula to the `formula` argument in `add_model()`:
#'
#'    ``` r
#'    library(workflows)
#'
#'    wflow <-
#'      workflow() |>
#'      add_formula(preproc_formula) |>
#'      add_model(model_spec, formula = model_formula)
#'
#'    fit(wflow, data = mtcars)
#'    ```
#'
#'    The workflow will then pass the model formula to parsnip, using the
#'    preprocessor formula elsewhere. We would still use the preprocessing
#'    formula if we had added a recipe preprocessor using `add_recipe()`
#'    instead a formula via `add_formula()`.
#'
#' }
#'
#' @name model_formula
NULL
