#' A wrapper function for conditional inference tree models
#'
#' These functions are slightly different APIs for [partykit::ctree()] and
#' [partykit::cforest()] that have several important arguments as top-level
#' arguments (as opposed to being specified in [partykit::ctree_control()]).
#' @param formula A symbolic description of the model to be fit.
#' @param data A data frame containing the variables in the model.
#' @param weights A vector of weights whose length is the same as `nrow(data)`.
#' For [partykit::ctree()] models, these are required to be non-negative
#' integers while for [partykit::cforest()] they can be non-negative integers
#' or doubles.
#' @param teststat A character specifying the type of the test statistic to be
#' applied.
#' @param testtype A character specifying how to compute the distribution of
#' the test statistic.
#' @param mincriterion The value of the test statistic (for \code{testtype ==
#' "Teststatistic"}), or 1 - p-value (for other values of \code{testtype}) that
#' must be exceeded in order to implement a split.
#' @param minsplit The minimum sum of weights in a node in order to be
#' considered for splitting.
#' @param maxdepth maximum depth of the tree. The default \code{maxdepth = Inf}
#' means that no restrictions are applied to tree sizes.
#' @param mtry Number of input variables randomly sampled as candidates at each
#' node for random forest like algorithms. The default \code{mtry = Inf} means
#' that no random selection takes place.
#' @param ntree Number of trees to grow in a forest.
#' @param ... Other options to pass to [partykit::ctree()] or [partykit::cforest()].
#' @return An object of class `party` (for `ctree`) or `cforest`.
#' @examplesIf !parsnip:::is_cran_check()
#' if (rlang::is_installed(c("modeldata", "partykit"))) {
#'   data(bivariate, package = "modeldata")
#'   ctree_train(Class ~ ., data = bivariate_train)
#'   ctree_train(Class ~ ., data = bivariate_train, maxdepth = 1)
#' }
#' @export
ctree_train <-
  function(formula,
           data,
           weights = NULL,
           minsplit = 20L,
           maxdepth = Inf,
           teststat = "quadratic",
           testtype = "Bonferroni",
           mincriterion = 0.95,
           ...) {
    rlang::check_installed("partykit")
    opts <- rlang::list2(...)

    if (any(names(opts) == "control")) {
      opts$control$minsplit <- minsplit
      opts$control$maxdepth <- maxdepth
      opts$control$teststat <- teststat
      opts$control$testtype <- testtype
      opts$control$mincriterion <- mincriterion
    } else {
      opts$control <-
        rlang::call2(
          "ctree_control",
          .ns = "partykit",
          !!!list(
            minsplit = minsplit,
            maxdepth = maxdepth,
            teststat = teststat,
            testtype = testtype,
            mincriterion = mincriterion
          )
        )
    }

    tree_call <-
      rlang::call2(
        "ctree",
        .ns = "partykit",
        formula = rlang::expr(formula),
        data = rlang::expr(data),
        !!!opts
      )
    if (!is.null(weights)) {
      if (!is.vector(weights) ||
          !is.integer(weights) ||
          length(weights) != nrow(data)) {
        cli::cli_abort(
          "{.arg weights} should be an integer vector with size the same
           as the number of rows of {.arg data}."
        )
      }
      tree_call$weights <- rlang::expr(weights)
    }

    eval_env <- rlang::env()

    environment(formula) <- rlang::new_environment(
      data = list(data = data, weights = weights),
      parent = environment(formula)
    )

    eval_env$data <- data
    eval_env$formula <- formula
    eval_env$weights <- weights
    rlang::eval_tidy(tree_call, env = eval_env)
  }

#' @rdname ctree_train
#' @export
cforest_train <-
  function(formula,
           data,
           weights = NULL,
           minsplit = 20L,
           maxdepth = Inf,
           teststat = "quadratic",
           testtype = "Univariate",
           mincriterion = 0,
           mtry = ceiling(sqrt(ncol(data) - 1)),
           ntree = 500L,
           ...) {
    rlang::check_installed("partykit")
    force(mtry)
    opts <- rlang::list2(...)

    mtry     <- max_mtry_formula(mtry, formula, data)
    minsplit <- min(minsplit, nrow(data))

    if (any(names(opts) == "control")) {
      opts$control$minsplit <- minsplit
      opts$control$maxdepth <- maxdepth
      opts$control$teststat <- teststat
      opts$control$testtype <- testtype
      opts$control$logmincriterion <- log(mincriterion)
      opts$control$mincriterion <- mincriterion
    } else {
      opts$control <-
        rlang::call2(
          "ctree_control",
          .ns = "partykit",
          !!!list(
            minsplit = minsplit,
            maxdepth = maxdepth,
            teststat = teststat,
            testtype = testtype,
            mincriterion = mincriterion,
            saveinfo = FALSE
          )
        )
    }
    opts$mtry <- mtry
    opts$ntree <- ntree
    forest_call <-
      rlang::call2(
        "cforest",
        .ns = "partykit",
        formula = rlang::expr(formula),
        data = rlang::expr(data),
        weights = rlang::expr(weights),
        !!!opts
      )

    if (!is.null(weights)) {
      if (!is.vector(weights) ||
          !is.numeric(weights) ||
          length(weights) != nrow(data)) {
        cli::cli_abort(
          "{.arg weights} should be a numeric vector with size the same as
           the number of rows of {.arg data}."
        )
      }
      forest_call$weights <- rlang::expr(weights)
    }

    eval_env <- rlang::env()

    environment(formula) <- rlang::new_environment(
      data = list(data = data, weights = weights),
      parent = environment(formula)
    )

    eval_env$data <- data
    eval_env$formula <- formula
    eval_env$weights <- weights

    rlang::eval_tidy(forest_call, env = eval_env)
  }

# ------------------------------------------------------------------------------

#' Determine largest value of mtry from formula.
#' This function potentially caps the value of `mtry` based on a formula and
#' data set. This is a safe approach for survival and/or multivariate models.
#' @param mtry An initial value of `mtry` (which may be too large).
#' @param formula A model formula.
#' @param data The training set (data frame).
#' @return A value for `mtry`.
#' @examplesIf !parsnip:::is_cran_check()
#' # should be 9
#' max_mtry_formula(200, cbind(wt, mpg) ~ ., data = mtcars)
#' @export
max_mtry_formula <- function(mtry, formula, data) {
  preds <- stats::model.frame(formula, head(data))
  trms <- attr(preds, "terms")
  p <- ncol(attr(trms, "factors"))

  max(min(mtry, p), 1L)
}
