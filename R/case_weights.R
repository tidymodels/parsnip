#' Using case weights with parsnip
#'
#' Case weights are positive numeric values that influence how much each data
#' point has during the model fitting process. There are a variety of situations
#' where case weights can be used.
#'
#' tidymodels packages differentiate _how_ different types of case weights
#' should be used during the entire data analysis process, including
#' preprocessing data, model fitting, performance calculations, etc.
#'
#' The tidymodels packages require users to convert their numeric vectors to a
#' vector class that reflects how these should be used. For example, there are
#' some situations where the weights should not affect operations such as
#' centering and scaling or other preprocessing operations.
#'
#' The types of weights allowed in tidymodels are:
#'
#' * Frequency weights via [hardhat::frequency_weights()]
#' * Importance weights via [hardhat::importance_weights()]
#'
#' More types can be added by request.
#'
#' For parsnip, the [fit()] and [fit_xy()] functions contain a `case_weight`
#' argument that takes these data. For Spark models, the argument value should
#' be a character value.
#'
#' @name case_weights
#' @seealso [frequency_weights()], [importance_weights()], [fit()], [fit_xy()]
NULL

# ------------------------------------------------------------------------------

weights_to_numeric <- function(x, spec) {
  if (is.null(x)) {
    return(NULL)
  } else if (spec$engine == "spark") {
    # Spark wants a column name
    return(x)
  }

  to_int <- c("hardhat_frequency_weights")
  if (inherits(x, to_int)) {
    x <- as.integer(x)
  } else {
    x <- as.numeric(x)
  }
  x
}

patch_formula_environment_with_case_weights <- function(formula,
                                                        data,
                                                        case_weights) {
  # `lm()` and `glm()` and others use the original model function call to
  # construct a call for `model.frame()`. That will normally fail because the
  # formula has its own environment attached (usually the global environment)
  # and it will look there for a vector named 'weights'. To account
  # for this, we create a child of the `formula`'s environment and
  # stash the `weights` there with the expected name and then
  # reassign this as the `formula`'s environment
  environment(formula) <- rlang::new_environment(
    data = list(data = data, weights = case_weights),
    parent = environment(formula)
  )

  formula
}

# ------------------------------------------------------------------------------

#' Determine if case weights are used
#'
#' Not all modeling engines can incorporate case weights into their
#' calculations. This function can determine whether they can be used.
#'
#' @param spec A parsnip [model specification][model_spec].
#' @return A single logical.
#' @examples
#' case_weights_allowed(linear_reg())
#' case_weights_allowed(linear_reg(engine = "keras"))
#' @export
case_weights_allowed <- function(spec) {
  mod_type <- class(spec)[1]
  mod_eng <- spec$engine
  mod_mode <- spec$mode

  model_info <-
    get_from_env(paste0(mod_type, "_fit")) |>
    dplyr::filter(engine == mod_eng & mode == mod_mode)
  if (nrow(model_info) != 1) {
    cli::cli_abort(
      "Error in getting model information for model {mod_type} with
       engine {mod_eng} and mode {mod_mode}."
    )
  }
  # If weights are used, they are protected data arguments with the canonical
  # name 'weights' (although this may not be the model function's argument name).
  data_args <- model_info$value[[1]]$protect
  any(data_args == "weights")
}

has_weights <- function(env) {
  !is.null(env$weights)
}
