#' A wrapper function for conditional inference tree models
#'
#' These functions are slightly different APIs for [partykit::ctree()] and
#' [partykit::cforest()] that have several important arguments as top-level
#' arguments (as opposed to being specified in [partykit::ctree_control()]).
#' @param formula A symbolic description of the model to be fit.
#' @param data A data frame containing the variables in the model.
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
#' @examples
#' if (rlang::is_installed(c("modeldata", "partykit"))) {
#'   data(bivariate, package = "modeldata")
#'   ctree_train(Class ~ ., data = bivariate_train)
#'   ctree_train(Class ~ ., data = bivariate_train, maxdepth = 1)
#' }
#' @export
ctree_train <-
  function(formula,
           data,
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
    rlang::eval_tidy(tree_call)
  }

#' @rdname ctree_train
#' @export
cforest_train <-
  function(formula,
           data,
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

    mtry     <- min(mtry, ncol(data) - 1)
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
        !!!opts
      )
    rlang::eval_tidy(forest_call)
  }
