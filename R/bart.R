#' Bayesian additive regression trees (BART)
#'
#' @description
#'
#' `bart()` defines a tree ensemble model that uses Bayesian analysis to
#' assemble the ensemble. This function can fit classification and regression
#' models.
#'
#' \Sexpr[stage=render,results=rd]{parsnip:::make_engine_list("bart")}
#'
#' More information on how \pkg{parsnip} is used for modeling is at
#' \url{https://www.tidymodels.org/}.
#'
#' @inheritParams nearest_neighbor
#' @inheritParams boost_tree
#' @param prior_terminal_node_coef A coefficient for the prior probability that
#' a node is a terminal node. Values are usually between 0 and one with
#' a default of 0.95. This affects the baseline probability; smaller numbers
#' make the probabilities larger overall. See Details below.
#' @param prior_terminal_node_expo An exponent in the prior probability that
#' a node is a terminal node.  Values are usually non-negative with
#' a default of 2 This affects the rate that the prior probability decreases as
#' the depth of the tree increases. Larger values make deeper trees less likely.
#' @param prior_outcome_range A positive value that defines the width of a prior
#' that the predicted outcome is within a certain range. For regression it is
#' related to the observed range of the data; the prior is the number of standard
#' deviations of a Gaussian distribution defined by the observed range of the
#' data. For classification, it is defined as the range of +/-3 (assumed to be
#' on the logit scale). The default value is 2.
#'
#' @details
#' The prior for the terminal node probability is expressed as
#' `prior = a * (1 + d)^(-b)` where `d` is the depth of the node, `a` is
#' `prior_terminal_node_coef` and `b` is `prior_terminal_node_expo`. See the
#' Examples section below for an example graph of the prior probability of a
#' terminal node for different values of these parameters.
#'
#'
#' @templateVar modeltype bart
#' @template spec-details
#'
#' @template spec-references
#'
#' @seealso \Sexpr[stage=render,results=rd]{parsnip:::make_seealso_list("bart")}
#'
#' @examplesIf !parsnip:::is_cran_check()
#' show_engines("bart")
#'
#' bart(mode = "regression", trees = 5)
#'
#' # ------------------------------------------------------------------------------
#' # Examples for terminal node prior
#'
#' library(ggplot2)
#' library(dplyr)
#'
#' prior_test <- function(coef = 0.95, expo = 2, depths = 1:10) {
#'   tidyr::crossing(coef = coef, expo = expo, depth = depths) |>
#'     mutate(
#'       `terminial node prior` = coef * (1 + depth)^(-expo),
#'       coef = format(coef),
#'       expo = format(expo))
#' }
#'
#' prior_test(coef = c(0.05, 0.5, .95), expo = c(1/2, 1, 2)) |>
#'   ggplot(aes(depth, `terminial node prior`, col = coef)) +
#'   geom_line() +
#'   geom_point() +
#'   facet_wrap(~ expo)

#' @export
bart <-
  function(mode = "unknown", engine = "dbarts",
           trees = NULL, prior_terminal_node_coef = NULL,
           prior_terminal_node_expo = NULL,
           prior_outcome_range = NULL) {

    args <- list(
      trees = enquo(trees),
      prior_terminal_node_coef = enquo(prior_terminal_node_coef),
      prior_terminal_node_expo = enquo(prior_terminal_node_expo),
      prior_outcome_range = enquo(prior_outcome_range)
    )

    new_model_spec(
      "bart",
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

#' @method update bart
#' @rdname parsnip_update
#' @inheritParams bart
#' @param prior_terminal_node_coef A coefficient for the prior probability that
#' a node is a terminal node.
#' @param prior_terminal_node_expo An exponent in the prior probability that
#' a node is a terminal node.
#' @export
update.bart <-
  function(object,
           parameters = NULL,
           trees = NULL,
           prior_terminal_node_coef = NULL,
           prior_terminal_node_expo = NULL,
           prior_outcome_range = NULL,
           fresh = FALSE, ...) {

    args <- list(
      trees = enquo(trees),
      prior_terminal_node_coef = enquo(prior_terminal_node_coef),
      prior_terminal_node_expo = enquo(prior_terminal_node_expo),
      prior_outcome_range = enquo(prior_outcome_range)
    )

    update_spec(
      object = object,
      parameters = parameters,
      args_enquo_list = args,
      fresh = fresh,
      cls = "bart",
      ...
    )
  }

#' Developer functions for predictions via BART models
#' @name bart-internal
#' @inheritParams predict.model_fit
#' @param obj A parsnip object.
#' @param level Confidence level.
#' @param std_err Attach column for standard error of prediction or not.
#' @export
#' @keywords internal
dbart_predict_calc <- function(obj, new_data, type, level = 0.95, std_err = FALSE) {
  types <- c("numeric", "class", "prob", "conf_int", "pred_int")
  mod_mode <- obj$spec$mode
  lo <- (1 - level)/2
  hi <-  1 - lo

  if (type == "conf_int") {
    post_dist <- predict(obj$fit, new_data, type = "ev")
  } else {
    post_dist <- predict(obj$fit, new_data, type = "ppd")
  }

  if (type == "numeric") {
    res <- tibble::tibble(.pred = apply(post_dist, 2, mean, na.rm = TRUE))
  } else if (type == "class") {
    mn <- apply(post_dist, 2, mean, na.rm = TRUE)
    lvl <- ifelse(mn > 0.5, obj$lvl[2], obj$lvl[1])
    lvl <- factor(lvl, levels = obj$lvl)
    res <- tibble::tibble(.pred_class = lvl)
  } else if (type == "prob") {
    mn <- apply(post_dist, 2, mean, na.rm = TRUE)
    res <-
      tibble::tibble(a = 1 - mn, b = mn) |>
      setNames(paste0(".pred_", obj$lv))
  } else if (type %in% c("conf_int", "pred_int")) {
    if (mod_mode == "regression") {
      res <-
        tibble::tibble(
          .pred_lower = apply(post_dist, 2, quantile, probs = lo, na.rm = TRUE),
          .pred_upper = apply(post_dist, 2, quantile, probs = hi, na.rm = TRUE)
        )
    } else {
      bnds <- apply(post_dist, 2, quantile, probs = c(lo, hi), na.rm = TRUE)
      bnds <- apply(bnds, 1, function(x) sort(x))

      res <-
        tibble::tibble(
          .pred_lower_a = 1 - bnds[,2],
          .pred_lower_b =     bnds[,1],
          .pred_upper_a = 1 - bnds[,1],
          .pred_upper_b =     bnds[,2]
        ) |>
        rlang::set_names(
          c(
            paste0(".pred_lower_", obj$lvl),
            paste0(".pred_upper_", obj$lvl)
          )
        )
    }
    if (std_err) {
      res$.std_error <- apply(post_dist, 2, stats::sd, na.rm = TRUE)
    }
  }
  res
}

