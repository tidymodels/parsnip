#' Partial least squares (PLS)
#'
#' @description
#' `pls()` defines a partial least squares model that uses latent variables to
#' model the data. It is similar to a supervised version of principal component.
#' This function can fit classification and regression models.
#'
#' \Sexpr[stage=render,results=rd]{parsnip:::make_engine_list("pls")}
#'
#' More information on how \pkg{parsnip} is used for modeling is at
#' \url{https://www.tidymodels.org/}.
#'
#' @inheritParams nearest_neighbor
#' @param predictor_prop The maximum proportion of original predictors that can
#'  have _non-zero_ coefficients for each PLS component (via regularization).
#'  This value is used for all PLS components for X.
#' @param num_comp The number of PLS components to retain.
#'
#' @templateVar modeltype pls
#' @template spec-details
#'
#' @template spec-references
#'
#' @seealso \Sexpr[stage=render,results=rd]{parsnip:::make_seealso_list("pls")}
#' @export
pls <-
  function(mode = "unknown", predictor_prop = NULL, num_comp = NULL, engine = "mixOmics") {

    args <- list(
      predictor_prop = enquo(predictor_prop),
      num_comp       = enquo(num_comp)
    )

    new_model_spec(
      "pls",
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

#' @export
#' @param object A PLS model specification.
#' @param parameters A 1-row tibble or named list with _main_
#'  parameters to update. If the individual arguments are used,
#'  these will supersede the values in `parameters`. Also, using
#'  engine arguments in this object will result in an error.
#' @param ... Not used for `update()`.
#' @param fresh A logical for whether the arguments should be
#'  modified in-place of or replaced wholesale.
#' @examplesIf !parsnip:::is_cran_check()
#' model <- pls(predictor_prop =  0.1)
#' model
#' update(model, predictor_prop = 1)
#' update(model, predictor_prop = 1, fresh = TRUE)
#' @method update pls
#' @inheritParams pls
#' @rdname parsnip_update
#' @export
update.pls <-
  function(object,
           parameters = NULL,
           predictor_prop = NULL, num_comp = NULL,
           fresh = FALSE, ...) {

    args <- list(
      predictor_prop    = enquo(predictor_prop),
      num_comp  = enquo(num_comp)
    )

    update_spec(
      object = object,
      parameters = parameters,
      args_enquo_list = args,
      fresh = fresh,
      cls = "pls",
      ...
    )
  }

# ------------------------------------------------------------------------------

#' @export
check_args.pls <- function(object, call = rlang::caller_env()) {

  args <- lapply(object$args, rlang::eval_tidy)

  check_number_whole(args$num_comp, min = 0, allow_null = TRUE, call = call, arg = "num_comp")

  invisible(object)
}


# ------------------------------------------------------------------------------

set_new_model("pls")
set_model_mode("pls", "regression")
set_model_mode("pls", "classification")
