#' Ensembles of MARS models
#'
#' @description
#'
#' `bag_mars()` defines an ensemble of generalized linear models that use
#' artificial features for some predictors. These features resemble hinge
#' functions and the result is a model that is a segmented regression in small
#' dimensions. This function can fit classification and regression models.
#'
#' \Sexpr[stage=render,results=rd]{parsnip:::make_engine_list("bag_mars")}
#'
#' More information on how \pkg{parsnip} is used for modeling is at
#' \url{https://www.tidymodels.org/}.
#'
#' @inheritParams mars
#'
#' @template spec-details
#'
#' @template spec-references
#'
#' @seealso \Sexpr[stage=render,results=rd]{parsnip:::make_seealso_list("bag_mars")}
#' @export
bag_mars <-
  function(mode = "unknown",
           num_terms = NULL,
           prod_degree = NULL,
           prune_method = NULL,
           engine = "earth") {
    args <- list(
      num_terms   = enquo(num_terms),
      prod_degree  = enquo(prod_degree),
      prune_method  = enquo(prune_method)
    )

    new_model_spec(
      "bag_mars",
      args = args,
      eng_args = NULL,
      mode = mode,
      method = NULL,
      engine = engine
    )
  }

#' @export
print.bag_mars <- function(x, ...) {
  cat("Bagged MARS Model Specification (", x$mode, ")\n\n", sep = "")
  model_printer(x, ...)

  if (!is.null(x$method$fit$args)) {
    cat("Model fit template:\n")
    print(show_call(x))
  }
  invisible(x)
}

# ------------------------------------------------------------------------------

#' @method update bag_mars
#' @rdname parsnip_update
#' @inheritParams mars
#' @export
update.bag_mars <-
  function(object,
           parameters = NULL,
           num_terms = NULL, prod_degree = NULL, prune_method = NULL,
           fresh = FALSE, ...) {

    eng_args <- update_engine_parameters(object$eng_args, ...)

    if (!is.null(parameters)) {
      parameters <- check_final_param(parameters)
    }
    args <- list(
      num_terms   = enquo(num_terms),
      prod_degree  = enquo(prod_degree),
      prune_method  = enquo(prune_method)
    )

    args <- update_main_parameters(args, parameters)

    if (fresh) {
      object$args <- args
      object$eng_args <- eng_args
    } else {
      null_args <- map_lgl(args, null_value)
      if (any(null_args))
        args <- args[!null_args]
      if (length(args) > 0)
        object$args[names(args)] <- args
      if (length(eng_args) > 0)
        object$eng_args[names(eng_args)] <- eng_args
    }

    new_model_spec(
      "bag_mars",
      args = object$args,
      eng_args = object$eng_args,
      mode = object$mode,
      method = NULL,
      engine = object$engine
    )
  }

# ------------------------------------------------------------------------------

set_new_model("bag_mars")
set_model_mode("bag_mars", "classification")
set_model_mode("bag_mars", "regression")
