#' Generalized additive models (GAMs)
#'
#' @description
#' `gen_additive_mod()` defines a model that can use smoothed functions of
#' numeric predictors in a generalized linear model. This function can fit
#' classification and regression models.
#'
#' `Rd parsnip:::make_engine_list("gen_additive_mod")`
#'
#' More information on how \pkg{parsnip} is used for modeling is at
#' \url{https://www.tidymodels.org/}.
#'
#' @inheritParams nearest_neighbor
#' @param select_features `TRUE` or `FALSE.` If `TRUE`, the model has the
#'  ability to eliminate a predictor (via penalization). Increasing
#'  `adjust_deg_free` will increase the likelihood of removing predictors.
#' @param adjust_deg_free If `select_features = TRUE`, then acts as a multiplier
#'  for smoothness. Increase this beyond 1 to produce smoother models.
#'
#' @templateVar modeltype gen_additive_mod
#' @template spec-details
#'
#' @template spec-references
#'
#' @seealso `Rd parsnip:::make_seealso_list("gen_additive_mod")`
#'
#' @examplesIf !parsnip:::is_cran_check()
#' show_engines("gen_additive_mod")
#'
#' gen_additive_mod()
#'
#' @export
gen_additive_mod <- function(
  mode = "unknown",
  select_features = NULL,
  adjust_deg_free = NULL,
  engine = "mgcv"
) {
  args <- list(
    select_features = rlang::enquo(select_features),
    adjust_deg_free = rlang::enquo(adjust_deg_free)
  )

  new_model_spec(
    "gen_additive_mod",
    args = args,
    eng_args = NULL,
    mode = mode,
    user_specified_mode = !missing(mode),
    method = NULL,
    engine = engine,
    user_specified_engine = !missing(engine)
  )
}

#' @export
#' @rdname parsnip_update
#' @inheritParams gen_additive_mod
update.gen_additive_mod <- function(
  object,
  select_features = NULL,
  adjust_deg_free = NULL,
  parameters = NULL,
  fresh = FALSE,
  ...
) {
  args <- list(
    select_features = rlang::enquo(select_features),
    adjust_deg_free = rlang::enquo(adjust_deg_free)
  )

  update_spec(
    object = object,
    parameters = parameters,
    args_enquo_list = args,
    fresh = fresh,
    cls = "gen_additive_mod",
    ...
  )
}


#' @export
translate.gen_additive_mod <- function(x, engine = x$engine, ...) {
  if (is.null(engine)) {
    message("Used `engine = 'mgcv'` for translation.")
    engine <- "gam"
  }
  x <- translate.default(x, engine, ...)

  if (engine == "vgam") {
    link_arg <- x$method$fit$args$link
    if (rlang::is_quosure(link_arg)) {
      link_arg <- rlang::eval_tidy(link_arg)
    }
    if (!is.null(link_arg)) {
      x$method$fit$args$link <- match_ordinal_link_vglm(link_arg)
    }

    family_arg <- x$method$fit$args$family
    if (rlang::is_quosure(family_arg)) {
      family_arg <- rlang::eval_tidy(family_arg)
    }
    if (!is.null(family_arg)) {
      x$method$fit$args$family <- match_ordinal_family(family_arg)
    }

    thresh_arg <- x$method$fit$args$Thresh
    if (rlang::is_quosure(thresh_arg)) {
      thresh_arg <- rlang::eval_tidy(thresh_arg)
    }
    if (!is.null(thresh_arg)) {
      x$method$fit$args$Thresh <- match_threshold_structure_vglm(thresh_arg)
    }

    # `acat()` does not support certain link functions
    check_ordinal_link_family_vglm(
      family = x$method$fit$args$family,
      link = x$method$fit$args$link
    )
  }

  x
}

#' @export
#' @keywords internal
fit_xy.gen_additive_mod <- function(object, ...) {
  trace <- rlang::trace_back()

  if ("workflows" %in% trace$namespace & identical(object$engine, "mgcv")) {
    cli::cli_abort(
      c(
        "!" = "When working with generalized additive models, please supply the
               model specification to {.fun workflows::add_model} along with a \\
               {.arg formula} argument.",
        "i" = "See {.help parsnip::model_formula} to learn more."
      ),
      call = NULL
    )
  }

  if (identical(object$engine, "mgcv")) {
    cli::cli_abort(c(
      "!" = "Please use {.fun fit} rather than {.fun fit_xy} to train \\
           generalized additive models with the {.val mgcv} engine.",
      "i" = "See {.help model_formula} to learn more."
    ))
  }

  # allow fitting GAMs that specify smooths via other arguments to use
  # `fit_xy()` (#775)
  NextMethod()
}
