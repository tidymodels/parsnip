#' Bayesian additive regression trees (BART)
#'
#' @description
#'
#' `bart()` defines a tree ensemble model that uses Bayesian analysis to
#' assemble the ensemble.
#'
#' There are different ways to fit this model. See the engine-specific pages
#' for more details:
#'
#' \Sexpr[stage=render,results=rd]{parsnip:::make_engine_list("bart")}
#'
#' More information on how \pkg{parsnip} is used for modeling is at
#' \url{https://www.tidymodels.org/}.
#'
#' @inheritParams boost_tree
#' @param prior_terminal_node_coef A coefficient for the prior probability that
#' a node is a terminal node. In the expression `prior = a * (1 + d)^(-b)`, this
#' parameter is the `a` coefficient. Values are usually between 0 and one with
#' a default of 0.95. This effects the baseline probability; smaller numbers
#' makes the probabilities larger overall.
#' @param prior_terminal_node_expo An exponent in the prior probability that
#' a node is a terminal node. In the expression `prior = a * (1 + d)^(-b)`, this
#' parameter is the `b` coefficient. Values are usually non-negative with
#' a default of 2 This effects the rate that the prior probability decreases as
#' the depth of the tree increases. Larger values make deeper trees more
#' unlikely.
#' @param prior_outcome_range A positive value that defines the width of a prior
#' that the predicted outcome is within a certain range. For regression it is
#' related to the observed range of the data; the prior is the number of standard
#' deviations of a Gaussian distribution defined by the observed range of the
#' data. For classification, it is defined as the range of +/-3 (assumed to be
#' on the logit scale). The default value is 2.
#'
#' @template spec-details
#'
#' @template spec-references
#'
#' @seealso \Sexpr[stage=render,results=rd]{parsnip:::make_seealso_list("bart")}
#'
#' @examples
#' show_engines("bart")
#'
#' bart(mode = "regression", num_terms = 5)
#'
#' # ------------------------------------------------------------------------------
#' # Examples for terminal node prior
#'
#' prior_test <- function(a = 0.95, b = 2, depths = 0:10) {
#'   tidyr::crossing(a = a, b = b, depth = depths)  %>%
#'     mutate(
#'       `terminial node prior` =  a * (1 + depth)^(-b),
#'       a = format(a),
#'       b = format(b))
#' }
#'
#' prior_test(a = c(0.05, 0.5, .95), b = 1:2) %>%
#'   ggplot(aes(depth, `terminial node prior`, col = a)) +
#'   geom_line() +
#'   geom_point() +
#'   facet_wrap(~ b) +
#'   scale_y_continuous(trans = "log")

#' @export
bart <-
  function(mode = "unknown", engine = "bartMachine",
           trees = NULL, prior_terminal_node_coef = NULL,
           prior_terminal_node_expo = NULL,
           prior_outcome_range = NULL) {

    args <- list(
      trees    = enquo(trees),
      prior_terminal_node_coef  = enquo(prior_terminal_node_coef),
      prior_terminal_node_expo = enquo(prior_outcome_range),
      prior_outcome_range = enquo(prior_terminal_node_expo)
    )

    new_model_spec(
      "bart",
      args = args,
      eng_args = NULL,
      mode = mode,
      method = NULL,
      engine = engine
    )
  }

#' @export
print.bart <- function(x, ...) {
  cat("BART Model Specification (", x$mode, ")\n\n", sep = "")
  model_printer(x, ...)

  if(!is.null(x$method$fit$args)) {
    cat("Model fit template:\n")
    print(show_call(x))
  }
  invisible(x)
}

# ------------------------------------------------------------------------------

#' @method update bart
#' @rdname parsnip_update
#' @export
update.bart <-
  function(object,
           parameters = NULL,
           trees = NULL,
           prior_terminal_node_coef = NULL,
           prior_terminal_node_expo = NULL,
           prior_outcome_range = NULL,
           fresh = FALSE, ...) {

    eng_args <- update_engine_parameters(object$eng_args, ...)

    if (!is.null(parameters)) {
      parameters <- check_final_param(parameters)
    }

    args <- list(
      trees    = enquo(trees),
      prior_terminal_node_coef  = enquo(prior_terminal_node_coef),
      prior_terminal_node_expo = enquo(prior_terminal_node_expo),
      prior_outcome_range = enquo(prior_outcome_range)
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
      "bart",
      args = object$args,
      eng_args = object$eng_args,
      mode = object$mode,
      method = NULL,
      engine = object$engine
    )
  }


#' @export
#' @keywords internal
bart_interval_calc <- function(new_data, obj, ci = TRUE, level = 0.95) {
  if (obj$spec$mode == "classification") {
    rlang::abort("In bartMachine: Prediction intervals are not possible for classification")
  }

  if (ci) {
    cl <-
      rlang::call2(
        "calc_credible_intervals",
        .ns = "bartMachine",
        bart_machine = rlang::expr(obj$fit),
        new_data = rlang::expr(new_data),
        ci_conf = level
      )

  } else {
    cl <-
      rlang::call2(
        "calc_prediction_intervals",
        .ns = "bartMachine",
        bart_machine = rlang::expr(obj$fit),
        new_data = rlang::expr(new_data),
        pi_conf = level
      )
  }
  res <- rlang::eval_tidy(cl)
  if (!ci) {
    res <- res$interval
  }
  res <- tibble::as_tibble(res)
  names(res) <- c(".pred_lower", ".pred_upper")
  res
}
