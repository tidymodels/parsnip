#' Multinomial regression
#'
#' @description
#'
#' `multinom_reg()` defines a model that uses linear predictors to predict
#' multiclass data using the multinomial distribution. This function can fit
#' classification models.
#'
#' \Sexpr[stage=render,results=rd]{parsnip:::make_engine_list("multinom_reg")}
#'
#' More information on how \pkg{parsnip} is used for modeling is at
#' \url{https://www.tidymodels.org/}.
#'
#' @param mode A single character string for the type of model.
#'  The only possible value for this model is "classification".
#' @param engine A single character string specifying what computational engine
#'  to use for fitting. Possible engines are listed below. The default for this
#'  model is `"nnet"`.
#' @param penalty A non-negative number representing the total
#'  amount of regularization (specific engines only).
#'  For `keras` models, this corresponds to purely L2 regularization
#'  (aka weight decay) while the other models can be a combination
#'  of L1 and L2 (depending on the value of `mixture`).
#' @param mixture A number between zero and one (inclusive) giving the
#'  proportion of L1 regularization (i.e. lasso) in the model.
#'
#'  * `mixture = 1` specifies a pure lasso model,
#'  * `mixture = 0`  specifies a ridge regression model, and
#'  * `0 < mixture < 1` specifies an elastic net model, interpolating lasso and ridge.
#'
#'  Available for specific engines only.
#'
#' @templateVar modeltype multinom_reg
#' @template spec-details
#'
#' @details This model fits a classification model for multiclass outcomes; for
#' binary outcomes, see [logistic_reg()].
#'
#' @template spec-references
#'
#' @seealso \Sexpr[stage=render,results=rd]{parsnip:::make_seealso_list("multinom_reg")}
#'
#' @examplesIf !parsnip:::is_cran_check()
#' show_engines("multinom_reg")
#'
#' multinom_reg()
#' @export
multinom_reg <-
  function(mode = "classification",
           engine = "nnet",
           penalty = NULL,
           mixture = NULL) {

    args <- list(
      penalty = enquo(penalty),
      mixture = enquo(mixture)
    )

    new_model_spec(
      "multinom_reg",
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
translate.multinom_reg <- translate.linear_reg

# ------------------------------------------------------------------------------

#' @method update multinom_reg
#' @rdname parsnip_update
#' @export
update.multinom_reg <-
  function(object,
           parameters = NULL,
           penalty = NULL, mixture = NULL,
           fresh = FALSE, ...) {

    args <- list(
      penalty = enquo(penalty),
      mixture = enquo(mixture)
    )

    update_spec(
      object = object,
      parameters = parameters,
      args_enquo_list = args,
      fresh = fresh,
      cls = "multinom_reg",
      ...
    )
  }

# ------------------------------------------------------------------------------

#' @export
check_args.multinom_reg <- function(object, call = rlang::caller_env()) {

  args <- lapply(object$args, rlang::eval_tidy)

  check_number_decimal(args$mixture, min = 0, max = 1, allow_null = TRUE, call = call, arg = "mixture")
  check_number_decimal(args$penalty, min = 0, allow_null = TRUE, call = call, arg = "penalty")

  invisible(object)
}

# ------------------------------------------------------------------------------

organize_nnet_prob <- function(x, object) {
  if (is.null(nrow(x))) {
    x_names <- names(x)
    x <- matrix(x, nrow = 1)
    colnames(x) <- x_names
  }
  format_classprobs(x)
}
