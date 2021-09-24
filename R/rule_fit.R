#' RuleFit models
#'
#' @description
#' `rule_fit()` defines a model that derives simple feature rules from a tree
#' ensemble and uses them as features to a regularized model.
#'
#' There are different ways to fit this model. The method of estimation is
#' chosen by setting the model _engine_.
#'
#' \Sexpr[stage=render,results=rd]{parsnip:::make_engine_list("rule_fit", pkg = "rules")}
#'
#' More information on how \pkg{parsnip} is used for modeling is at
#' \url{https://www.tidymodels.org/}.
#'
#' @inheritParams boost_tree
#' @param mtry An number for the number (or proportion) of predictors that will
#'  be randomly sampled at each split when creating the tree models.
#' @param trees An integer for the number of trees contained in
#'  the ensemble.
#' @param min_n An integer for the minimum number of data points
#'  in a node that are required for the node to be split further.
#' @param tree_depth An integer for the maximum depth of the tree (i.e. number
#'  of splits).
#' @param learn_rate A number for the rate at which the boosting algorithm adapts
#'   from iteration-to-iteration.
#' @param loss_reduction A number for the reduction in the loss function required
#'   to split further .
#' @param sample_size An number for the number (or proportion) of data that is
#'  exposed to the fitting routine.
#' @param penalty L1 regularization parameter.
#' @details
#' The RuleFit model creates a regression model of rules in two stages. The
#'  first stage uses a tree-based model that is used to generate a set of rules
#'  that can be filtered, modified, and simplified. These rules are then added
#'  as predictors to a regularized generalized linear model that can also
#'  conduct feature selection during model training.
#'
#' @references Friedman, J. H., and Popescu, B. E. (2008). "Predictive learning
#' via rule ensembles." _The Annals of Applied Statistics_, 2(3), 916-954.
#'
#' @template spec-details
#'
#' @template spec-references
#'
#' @seealso [xrf::xrf.formula()], \Sexpr[stage=render,results=rd]{parsnip:::make_seealso_list("rule_fit", "rules")}
#'
#' @examples
#' show_engines("rule_fit")
#'
#' rule_fit()
#'
#' @export
#' @importFrom purrr map_lgl
rule_fit <-
  function(mode = "unknown",
           mtry = NULL, trees = NULL, min_n = NULL,
           tree_depth = NULL, learn_rate = NULL,
           loss_reduction = NULL,
           sample_size = NULL,
           penalty = NULL,
           engine = "xrf") {

    args <- list(
      mtry = enquo(mtry),
      trees = enquo(trees),
      min_n = enquo(min_n),
      tree_depth = enquo(tree_depth),
      learn_rate = enquo(learn_rate),
      loss_reduction = enquo(loss_reduction),
      sample_size = enquo(sample_size),
      penalty = enquo(penalty)
    )


    new_model_spec(
      "rule_fit",
      args = args,
      eng_args = NULL,
      mode = mode,
      method = NULL,
      engine = engine
    )
  }

#' @export
print.rule_fit <- function(x, ...) {
  cat("RuleFit Model Specification (", x$mode, ")\n\n", sep = "")
  model_printer(x, ...)

  if (!is.null(x$method$fit$args)) {
    cat("Model fit template:\n")
    print(show_call(x))
  }

  invisible(x)
}


# ------------------------------------------------------------------------------

#' @param object A `rule_fit` model specification.
#' @examples
#' # ------------------------------------------------------------------------------
#'
#' model <- rule_fit(trees = 10, min_n = 2)
#' model
#' update(model, trees = 1)
#' update(model, trees = 1, fresh = TRUE)
#' @method update rule_fit
#' @rdname rules_update
#' @inheritParams rules_update
#' @inheritParams rule_fit
#' @export
update.rule_fit <-
  function(object,
           parameters = NULL,
           mtry = NULL, trees = NULL, min_n = NULL,
           tree_depth = NULL, learn_rate = NULL,
           loss_reduction = NULL, sample_size = NULL,
           penalty = NULL,
           fresh = FALSE, ...) {
    update_dot_check(...)

    if (!is.null(parameters)) {
      parameters <- check_final_param(parameters)
    }

    args <- list(
      mtry = enquo(mtry),
      trees = enquo(trees),
      min_n = enquo(min_n),
      tree_depth = enquo(tree_depth),
      learn_rate = enquo(learn_rate),
      loss_reduction = enquo(loss_reduction),
      sample_size = enquo(sample_size),
      penalty = enquo(penalty)
    )

    args <- update_main_parameters(args, parameters)

    if (fresh) {
      object$args <- args
    } else {
      null_args <- map_lgl(args, null_value)
      if (any(null_args))
        args <- args[!null_args]
      if (length(args) > 0)
        object$args[names(args)] <- args
    }

    new_model_spec(
      "rule_fit",
      args = object$args,
      eng_args = object$eng_args,
      mode = object$mode,
      method = NULL,
      engine = object$engine
    )
  }


#' @rdname multi_predict
#' @export
#' @param penalty Non-negative penalty values.
#' @param ... Not currently used.
multi_predict._xrf <-
  function(object, new_data, type = NULL, penalty = NULL, ...) {
    if (any(names(enquos(...)) == "newdata")) {
      rlang::abort("Did you mean to use `new_data` instead of `newdata`?")
    }
    if (is.null(penalty)) {
      penalty <- object$fit$lambda
    }

    if (is.null(type)) {
      fam <- object$fit$family
      if (fam %in% c("binomial", "multinomial")) {
        type <- "class"
      } else {
        type <- "numeric"
      }
    }

    new_data <- prepare_data(object, new_data)
    # preprocess data
    if (!is.null(object$spec$method$pred$numeric$pre)) {
      new_data <- object$spec$method$pred$numeric$pre(new_data, object)
    }

    res <- xrf_pred(object, new_data, lambda = penalty, type = type, ...)
    res
  }

#' @export
#' @keywords internal
#' @rdname tunable-parsnip
tunable.rule_fit <- function(x, ...) {
  tibble::tibble(
    name = c('mtry', 'trees', 'min_n', 'tree_depth', 'learn_rate',
             'loss_reduction', 'sample_size', 'penalty'),
    call_info = list(
      list(pkg = "rules", fun = "mtry_prop"),
      list(pkg = "dials", fun = "trees", range = c(5L, 100L)),
      list(pkg = "dials", fun = "min_n"),
      list(pkg = "dials", fun = "tree_depth", range = c(1L, 10L)),
      list(pkg = "dials", fun = "learn_rate", range = c(-10, 0)),
      list(pkg = "dials", fun = "loss_reduction"),
      list(pkg = "dials", fun = "sample_prop", range = c(0.50, 0.95)),
      list(pkg = "dials", fun = "penalty")
    ),
    source = "model_spec",
    component = class(x)[class(x) != "model_spec"][1],
    component_id =  "main"
  )
}

# ------------------------------------------------------------------------------

set_new_model("rule_fit")
set_model_mode("rule_fit", "classification")
set_model_mode("rule_fit", "regression")
