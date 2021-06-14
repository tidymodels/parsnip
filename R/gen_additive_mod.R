# gen_additive_mod() - General Interface to Linear GAM Models
# - backend: gam
# - prediction:
#   - mode = "regression" (default) uses
#   - mode = "classification"

#' Generalized additive models (GAMs)
#'
#' `gen_additive_mod()` defines a model that can use smoothed functions of
#' numeric predictors in a generalized linear model.
#'
#' There are different ways to fit this model. See the engine-specific pages
#' for more details
#'
#' More information on how `parsnip` is used for modeling is at
#' \url{https://www.tidymodels.org}.
#'
#' @inheritParams boost_tree
#' @param select_features TRUE or FALSE. If this is TRUE then can add an
#'  extra penalty to each term so that it can be penalized to zero.
#'  This means that the smoothing parameter estimation that is part of
#'  fitting can completely remove terms from the model. If the corresponding
#'  smoothing parameter is estimated as zero then the extra penalty has no effect.
#'  Use `adjust_deg_free` to increase level of penalization.
#' @param adjust_deg_free If `select_features = TRUE`, then acts as a multiplier for smoothness.
#'  Increase this beyond 1 to produce smoother models.
#'
#'
#' @return
#' A `parsnip` model specification
#'
#' @details
#'
#' This function only defines what _type_ of model is being fit. Once an engine
#'  is specified, the _method_ to fit the model is also defined.
#'
#' The model is not trained or fit until the [fit.model_spec()] function is used
#' with the data.
#'
#' __gam__
#'
#' This engine uses [mgcv::gam()] and has the following parameters,
#' which can be modified through the [set_engine()] function.
#'
#' ``` {r echo=F}
#' str(mgcv::gam)
#' ```
#'
#' @section Fit Details:
#'
#' __MGCV Formula Interface__
#'
#' Fitting GAMs is accomplished using parameters including:
#'
#' - [mgcv::s()]: GAM spline smooths
#' - [mgcv::te()]: GAM tensor product smooths
#'
#' These are applied in the `fit()` function:
#'
#' ``` r
#' fit(value ~ s(date_mon, k = 12) + s(date_num), data = df)
#' ```
#'
#' @references \url{https://www.tidymodels.org},
#' [_Tidy Models with R_](https://tmwr.org)
#' @examples
#'
#' #show_engines("gen_additive_mod")
#'
#' #gen_additive_mod()
#'
#'
#' @export
gen_additive_mod <- function(mode = "unknown",
                             select_features = NULL,
                             adjust_deg_free = NULL) {

  args <- list(
    select_features = rlang::enquo(select_features),
    adjust_deg_free = rlang::enquo(adjust_deg_free)
  )

  new_model_spec(
    "gen_additive_mod",
    args     = args,
    eng_args = NULL,
    mode     = mode,
    method   = NULL,
    engine   = NULL
  )

}

#' @export
print.gen_additive_mod <- function(x, ...) {
  cat("GAM Specification (", x$mode, ")\n\n", sep = "")
  model_printer(x, ...)

  if(!is.null(x$method$fit$args)) {
    cat("Model fit template:\n")
    print(show_call(x))
  }

  invisible(x)
}

#' @export
#' @rdname parsnip_update
#' @importFrom stats update
#' @inheritParams gen_additive_mod
update.gen_additive_mod <- function(object,
                                    select_features = NULL,
                                    adjust_deg_free = NULL,
                                    parameters = NULL,
                                    fresh = FALSE, ...) {

  update_dot_check(...)

  if (!is.null(parameters)) {
    parameters <- check_final_param(parameters)
  }

  args <- list(
    select_features = rlang::enquo(select_features),
    adjust_deg_free = rlang::enquo(adjust_deg_free)
  )

  args <- update_main_parameters(args, parameters)

  if (fresh) {
    object$args <- args
  } else {
    null_args <- purrr::map_lgl(args, null_value)
    if (any(null_args))
      args <- args[!null_args]
    if (length(args) > 0)
      object$args[names(args)] <- args
  }

  new_model_spec(
    "gen_additive_mod",
    args     = object$args,
    eng_args = object$eng_args,
    mode     = object$mode,
    method   = NULL,
    engine   = object$engine
  )
}


#' @export
translate.gen_additive_mod <- function(x, engine = x$engine, ...) {
  if (is.null(engine)) {
    message("Used `engine = 'mgcv'` for translation.")
    engine <- "gam"
  }
  x <- translate.default(x, engine, ...)

  x
}

#' @export
#' @keywords internal
fit_xy.gen_additive_mod <- function(object, ...) {
  rlang::abort("`fit()` must be used with GAM models (due to its use of formulas).")
}
