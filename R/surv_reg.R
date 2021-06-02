#' Parametric survival regression
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is soft-deprecated in favor of `survival_reg()` which uses the
#' `"censored regression"` mode.
#'
#' `surv_reg()` is a way to generate a _specification_ of a model
#'  before fitting and allows the model to be created using
#'  R. The main argument for the
#'  model is:
#' \itemize{
#'   \item \code{dist}: The probability distribution of the outcome.
#' }
#' This argument is converted to its specific names at the
#'  time that the model is fit. Other options and arguments can be
#'  set using `set_engine()`. If left to its default
#'  here (`NULL`), the value is taken from the underlying model
#'  functions.
#'
#' The data given to the function are not saved and are only used
#'  to determine the _mode_ of the model. For `surv_reg()`,the
#'  mode will always be "regression".
#'
#'  Since survival models typically involve censoring (and require the use of
#'  [survival::Surv()] objects), the [fit.model_spec()] function will require that the
#'  survival model be specified via the formula interface.
#'
#' Also, for the `flexsurv::flexsurvfit` engine, the typical
#'  `strata` function cannot be used. To achieve the same effect,
#'  the extra parameter roles can be used (as described above).
#'
#' @param mode A single character string for the type of model.
#'  The only possible value for this model is "regression".
#' @param dist A character string for the outcome distribution. "weibull" is
#'  the default.
#' @details
#' For `surv_reg()`, the mode will always be "regression".
#'
#' The model can be created using the `fit()` function using the
#'  following _engines_:
#' \itemize{
#' \item \pkg{R}:  `"flexsurv"`, `"survival"` (the default)
#' }
#'
#' @includeRmd man/rmd/surv-reg.Rmd details
#'
#' @seealso [fit.model_spec()], [survival::Surv()], [set_engine()], [update()]
#' @references Jackson, C. (2016). `flexsurv`: A Platform for Parametric Survival
#'  Modeling in R. _Journal of Statistical Software_, 70(8), 1 - 33.
#' @examples
#' show_engines("surv_reg")
#'
#' surv_reg()
#'
#' @keywords internal
#' @export
surv_reg <- function(mode = "regression", dist = NULL) {

  lifecycle::deprecate_soft("0.1.6", "surv_reg()", "survival_reg()")

    args <- list(
      dist = enquo(dist)
    )

    new_model_spec(
      "surv_reg",
      args = args,
      eng_args = NULL,
      mode = mode,
      method = NULL,
      engine = NULL
    )
  }

#' @export
print.surv_reg <- function(x, ...) {
  cat("Parametric Survival Regression Model Specification (", x$mode, ")\n\n", sep = "")
  model_printer(x, ...)

  if(!is.null(x$method$fit$args)) {
    cat("Model fit template:\n")
    print(show_call(x))
  }

  invisible(x)
}

# ------------------------------------------------------------------------------

#' @method update surv_reg
#' @rdname parsnip_update
#' @export
update.surv_reg <- function(object, parameters = NULL, dist = NULL, fresh = FALSE, ...) {

  eng_args <- update_engine_parameters(object$eng_args, ...)

  if (!is.null(parameters)) {
    parameters <- check_final_param(parameters)
  }

  args <- list(
    dist = enquo(dist)
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
    "surv_reg",
    args = object$args,
    eng_args = object$eng_args,
    mode = object$mode,
    method = NULL,
    engine = object$engine
  )
}


# ------------------------------------------------------------------------------

#' @export
translate.surv_reg <- function(x, engine = x$engine, ...) {
  if (is.null(engine)) {
    message("Used `engine = 'survival'` for translation.")
    engine <- "survival"
  }
  x <- translate.default(x, engine, ...)
  x
}

# ------------------------------------------------------------------------------

check_args.surv_reg <- function(object) {

  if (object$engine == "flexsurv") {

    args <- lapply(object$args, rlang::eval_tidy)

    # `dist` has no default in the function
    if (all(names(args) != "dist") || is.null(args$dist))
      object$args$dist <- "weibull"
  }

  invisible(object)
}

# ------------------------------------------------------------------------------

#' @importFrom stats setNames
#' @importFrom dplyr mutate
survreg_quant <- function(results, object) {
  pctl <- object$spec$method$pred$quantile$args$p
  n <- nrow(results)
  p <- ncol(results)
  colnames(results) <- names0(p)
  results <-
    results %>%
    as_tibble() %>%
    mutate(.row = 1:n) %>%
    gather(.label, .pred, -.row) %>%
    arrange(.row, .label) %>%
    mutate(.quantile = rep(pctl, n)) %>%
    dplyr::select(-.label)
  .row <- results[[".row"]]
  results <-
    results %>%
    dplyr::select(-.row)
  results <- split(results, .row)
  names(results) <- NULL
  tibble(.pred = results)
}

# ------------------------------------------------------------------------------

#' @importFrom dplyr bind_rows
flexsurv_mean <- function(results, object) {
  results <- unclass(results)
  results <- bind_rows(results)
  results$est
}

#' @importFrom stats setNames
flexsurv_quant <- function(results, object) {
  results <- map(results, as_tibble)
  names(results) <- NULL
  results <- map(results, setNames, c(".quantile", ".pred", ".pred_lower", ".pred_upper"))
}

