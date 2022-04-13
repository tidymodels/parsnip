#' Cubist rule-based regression models
#'
#' @description
#' `cubist_rules()` defines a model that derives simple feature rules from a tree
#' ensemble and creates regression models within each rule. This function can fit
#' regression models.
#'
#' \Sexpr[stage=render,results=rd]{parsnip:::make_engine_list("cubist_rules")}
#'
#' More information on how \pkg{parsnip} is used for modeling is at
#' \url{https://www.tidymodels.org/}.
#'
#' @inheritParams boost_tree
#' @param mode A single character string for the type of model.
#'  The only possible value for this model is "regression".
#' @param committees A non-negative integer (no greater than 100) for the number
#'  of members of the ensemble.
#' @param max_rules The largest number of rules.
#' @param neighbors An integer between zero and nine for the number of training
#' set instances that are used to adjust the model-based prediction.
#' @details
#' Cubist is a rule-based ensemble regression model. A basic model tree
#'  (Quinlan, 1992) is created that has a separate linear regression model
#'  corresponding for each terminal node. The paths along the model tree are
#'  flattened into rules and these rules are simplified and pruned. The parameter
#'  `min_n` is the primary method for controlling the size of each tree while
#'  `max_rules` controls the number of rules.
#'
#' Cubist ensembles are created using _committees_, which are similar to
#'  boosting. After the first model in the committee is created, the second
#'  model uses a modified version of the outcome data based on whether the
#'  previous model under- or over-predicted the outcome. For iteration _m_, the
#'  new outcome `y*` is computed using
#'
#' \figure{comittees.png}
#'
#' If a sample is under-predicted on the previous iteration, the outcome is
#'  adjusted so that the next time it is more likely to be over-predicted to
#'  compensate. This adjustment continues for each ensemble iteration. See
#'  Kuhn and Johnson (2013) for details.
#'
#' After the model is created, there is also an option for a post-hoc
#'  adjustment that uses the training set (Quinlan, 1993). When a new sample is
#'  predicted by the model, it can be modified by its nearest neighbors in the
#'  original training set. For _K_ neighbors, the model-based predicted value is
#'  adjusted by the neighbor using:
#'
#' \figure{adjust.png}
#'
#' where `t` is the training set prediction and `w` is a weight that is inverse
#'  to the distance to the neighbor.
#'
#' @template spec-details
#'
#' @template spec-references
#'
#' @seealso [Cubist::cubist()], [Cubist::cubistControl()], \Sexpr[stage=render,results=rd]{parsnip:::make_seealso_list("cubist_rules")}
#'
#' @references Quinlan R (1992). "Learning with Continuous Classes." Proceedings
#' of the 5th Australian Joint Conference On Artificial Intelligence, pp.
#' 343-348.
#'
#' Quinlan R (1993)."Combining Instance-Based and Model-Based Learning."
#' Proceedings of the Tenth International Conference on Machine Learning, pp.
#' 236-243.
#'
#' Kuhn M and Johnson K (2013). _Applied Predictive Modeling_. Springer.
#' @export
cubist_rules <-
  function(mode = "regression",
           committees = NULL,
           neighbors = NULL,
           max_rules = NULL,
           engine = "Cubist") {

    args <- list(
      committees = enquo(committees),
      neighbors = enquo(neighbors),
      max_rules = enquo(max_rules)
    )

    new_model_spec(
      "cubist_rules",
      args = args,
      eng_args = NULL,
      mode = mode,
      method = NULL,
      engine = engine
    )
  }

#' @export
print.cubist_rules <- function(x, ...) {
  cat("Cubist Model Specification (", x$mode, ")\n\n", sep = "")
  model_printer(x, ...)

  if (!is.null(x$method$fit$args)) {
    cat("Model fit template:\n")
    print(show_call(x))
  }

  invisible(x)
}


# ------------------------------------------------------------------------------

#' @param object A Cubist model specification.
#' @examples
#'
#' # ------------------------------------------------------------------------------
#'
#' model <- cubist_rules(committees = 10, neighbors = 2)
#' model
#' update(model, committees = 1)
#' update(model, committees = 1, fresh = TRUE)
#' @method update cubist_rules
#' @rdname parsnip_update
#' @inheritParams parsnip_update
#' @inheritParams cubist_rules
#' @export
update.cubist_rules <-
  function(object,
           parameters = NULL,
           committees = NULL, neighbors = NULL, max_rules = NULL,
           fresh = FALSE, ...) {

    eng_args <- update_engine_parameters(object$eng_args, ...)

    if (!is.null(parameters)) {
      parameters <- check_final_param(parameters)
    }
    args <- list(
      committees = enquo(committees),
      neighbors = enquo(neighbors),
      max_rules = enquo(max_rules)
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
      "cubist_rules",
      args = object$args,
      eng_args = object$eng_args,
      mode = object$mode,
      method = NULL,
      engine = object$engine
    )
  }

# ------------------------------------------------------------------------------

# make work in different places

check_args.cubist_rules <- function(object) {

  args <- lapply(object$args, rlang::eval_tidy)

  if (is.numeric(args$committees)) {
    if (length(args$committees) > 1) {
      rlang::abort("Only a single committee member is used.")
    }
    msg <- "The number of committees should be >= 1 and <= 100. Truncating the value."
    if (args$committees > 100) {
      object$args$committees <-
        rlang::new_quosure(100L, env = rlang::empty_env())
      rlang::warn(msg)
    }
    if (args$committees < 1) {
      object$args$committees <-
        rlang::new_quosure(1L, env = rlang::empty_env())
      rlang::warn(msg)
    }

  }
  if (is.numeric(args$neighbors)) {
    if (length(args$neighbors) > 1) {
      rlang::abort("Only a single neighbors value is used.")
    }
    msg <- "The number of neighbors should be >= 0 and <= 9. Truncating the value."
    if (args$neighbors > 9) {
      object$args$neighbors <-
        rlang::new_quosure(9L, env = rlang::empty_env())
      rlang::warn(msg)
    }
    if (args$neighbors < 0) {
      object$args$neighbors <-
        rlang::new_quosure(0L, env = rlang::empty_env())
      rlang::warn(msg)
    }

  }
  invisible(object)
}

# ------------------------------------------------------------------------------

set_new_model("cubist_rules")
set_model_mode("cubist_rules", "regression")
