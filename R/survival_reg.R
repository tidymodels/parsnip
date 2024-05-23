#' Parametric survival regression
#'
#' @description
#' `survival_reg()` defines a parametric survival model. This function can fit
#' censored regression models.
#'
#' \Sexpr[stage=render,results=rd]{parsnip:::make_engine_list("survival_reg")}
#'
#' More information on how \pkg{parsnip} is used for modeling is at
#' \url{https://www.tidymodels.org/}.
#'
#' @inheritParams nearest_neighbor
#' @param mode A single character string for the prediction outcome mode.
#'  The only possible value for this model is "censored regression".
#' @param dist A character string for the probability distribution of the
#'  outcome. The default is "weibull".
#'
#'
#' @templateVar modeltype survival_reg
#' @template spec-details
#'
#' @template spec-survival
#'
#' @template spec-references
#'
#' @seealso \Sexpr[stage=render,results=rd]{parsnip:::make_seealso_list("survival_reg")}
#'
#' @examplesIf !parsnip:::is_cran_check()
#' show_engines("survival_reg")
#'
#' survival_reg(mode = "censored regression", dist = "weibull")
#' @keywords internal
#' @export
survival_reg <- function(mode = "censored regression", engine = "survival", dist = NULL) {

  args <- list(
    dist = enquo(dist)
  )

  new_model_spec(
    "survival_reg",
    args = args,
    eng_args = NULL,
    mode = mode,
    user_specified_mode = !missing(mode),
    method = NULL,
    engine = engine,
    user_specified_engine = !missing(engine)
  )
}

# ------------------------------------------------------------------------------

#' @method update survival_reg
#' @rdname parsnip_update
#' @export
update.survival_reg <- function(object, parameters = NULL, dist = NULL, fresh = FALSE, ...) {

  args <- list(
    dist = enquo(dist)
  )

  update_spec(
    object = object,
    parameters = parameters,
    args_enquo_list = args,
    fresh = fresh,
    cls = "survival_reg",
    ...
  )
}


#' @export
translate.survival_reg <- function(x, engine = x$engine, ...) {
  if (is.null(engine)) {
    message("Used `engine = 'survival'` for translation.")
    engine <- "survival"
  }
  x <- translate.default(x, engine, ...)
  x
}

#' @export
check_args.survival_reg <- function(object, call = rlang::caller_env()) {

  if (object$engine == "flexsurv") {

    args <- lapply(object$args, rlang::eval_tidy)

    # `dist` has no default in the function
    if (all(names(args) != "dist") || is.null(args$dist))
      object$args$dist <- "weibull"
  }

  invisible(object)
}
