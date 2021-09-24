#' Partial least squares (PLS)
#'
#' @description
#' `pls()` defines a model that uses a supervised version of principal component
#' analysis that uses latent variables to model the data.
#'
#' There are different ways to fit this model. The method of estimation is
#' chosen by setting the model _engine_.
#'
#' \Sexpr[stage=render,results=rd]{parsnip:::make_engine_list("pls", pkg = "plsmod")}
#'
#' More information on how \pkg{parsnip} is used for modeling is at
#' \url{https://www.tidymodels.org/}.
#'
#' @inheritParams boost_tree
#' @param predictor_prop The maximum proportion of original predictors that can
#'  have _non-zero_ coefficients for each PLS component (via regularization).
#'  This value is used for all PLS components for X.
#' @param num_comp The number of PLS components to retain.
#'
#' @template spec-details
#'
#' @template spec-references
#'
#' @seealso \Sexpr[stage=render,results=rd]{parsnip:::make_seealso_list("pls", "plsmod")}
#' @export
pls <-
  function(mode = "unknown", predictor_prop = NULL, num_comp = NULL) {

    args <- list(
      predictor_prop = enquo(predictor_prop),
      num_comp       = enquo(num_comp)
    )

    new_model_spec(
      "pls",
      args = args,
      eng_args = NULL,
      mode = mode,
      method = NULL,
      engine = NULL
    )
  }

#' @export
print.pls <- function(x, ...) {
  cat("PLS Model Specification (", x$mode, ")\n\n", sep = "")
  model_printer(x, ...)

  if (!is.null(x$method$fit$args)) {
    cat("Model fit template:\n")
    print(show_call(x))
  }
  invisible(x)
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
#' @examples
#' model <- pls(predictor_prop =  0.1)
#' model
#' update(model, predictor_prop = 1)
#' update(model, predictor_prop = 1, fresh = TRUE)
#' @method update pls
#' @rdname pls
#' @export
update.pls <-
  function(object,
           parameters = NULL,
           predictor_prop = NULL, num_comp = NULL,
           fresh = FALSE, ...) {
    update_dot_check(...)

    if (!is.null(parameters)) {
      parameters <- check_final_param(parameters)
    }

    args <- list(
      predictor_prop    = enquo(predictor_prop),
      num_comp  = enquo(num_comp)
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
      "pls",
      args = object$args,
      eng_args = object$eng_args,
      mode = object$mode,
      method = NULL,
      engine = object$engine
    )
  }

# ------------------------------------------------------------------------------

check_args.pls <- function(object) {

  args <- lapply(object$args, rlang::eval_tidy)

  if (is.numeric(args$num_comp) && args$num_comp < 0)
    rlang::abort("`num_comp` should be >= 1.")

  invisible(object)
}


# ------------------------------------------------------------------------------

set_new_model("pls")
set_model_mode("pls", "regression")
set_model_mode("pls", "classification")
