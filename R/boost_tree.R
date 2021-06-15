# Prototype parsnip code for boosted trees

#' General Interface for Boosted Trees
#'
#' `boost_tree()` is a way to generate a _specification_ of a model
#'  before fitting and allows the model to be created using
#'  different packages in R or via Spark. The main arguments for the
#'  model are:
#' \itemize{
#'   \item \code{mtry}: The number of predictors that will be
#'   randomly sampled at each split when creating the tree models.
#'   \item \code{trees}: The number of trees contained in the ensemble.
#'   \item \code{min_n}: The minimum number of data points in a node
#'   that is required for the node to be split further.
#'   \item \code{tree_depth}: The maximum depth of the tree (i.e. number of
#'  splits).
#'   \item \code{learn_rate}: The rate at which the boosting algorithm adapts
#'   from iteration-to-iteration.
#'   \item \code{loss_reduction}: The reduction in the loss function required
#'   to split further.
#'   \item \code{sample_size}: The amount of data exposed to the fitting routine.
#'   \item \code{stop_iter}: The number of iterations without improvement before
#'   stopping.
#' }
#' These arguments are converted to their specific names at the
#'  time that the model is fit. Other options and arguments can be
#'  set using the `set_engine()` function. If left to their defaults
#'  here (`NULL`), the values are taken from the underlying model
#'  functions. If parameters need to be modified, `update()` can be used
#'  in lieu of recreating the object from scratch.
#'
#' @param mode A single character string for the type of model.
#'  Possible values for this model are "unknown", "regression", or
#'  "classification".
#' @param engine A character string for the method of fitting. Possible engines
#' are listed above. The default for this model is `"xgboost"`.
#' @param mtry A number for the number (or proportion) of predictors that will
#'  be randomly sampled at each split when creating the tree models (`xgboost`
#'  only).
#' @param trees An integer for the number of trees contained in
#'  the ensemble.
#' @param min_n An integer for the minimum number of data points
#'  in a node that is required for the node to be split further.
#' @param tree_depth An integer for the maximum depth of the tree (i.e. number
#'  of splits) (`xgboost` only).
#' @param learn_rate A number for the rate at which the boosting algorithm adapts
#'   from iteration-to-iteration (`xgboost` only).
#' @param loss_reduction A number for the reduction in the loss function required
#'   to split further (`xgboost` only).
#' @param sample_size A number for the number (or proportion) of data that is
#'  exposed to the fitting routine. For `xgboost`, the sampling is done at
#'  each iteration while `C5.0` samples once during training.
#' @param stop_iter The number of iterations without improvement before
#'   stopping (`xgboost` only).
#' @details
#' The data given to the function are not saved and are only used
#'  to determine the _mode_ of the model. For `boost_tree()`, the
#'  possible modes are "regression" and "classification".
#'
#' The model can be created using the `fit()` function using the
#'  following _engines_:
#' \itemize{
#' \item \pkg{R}: `"xgboost"` (the default), `"C5.0"`
#' \item \pkg{Spark}: `"spark"`
#' }
#'
#' For this model, other packages may add additional engines. Use
#' [show_engines()] to see the current set of engines.
#'
#' @includeRmd man/rmd/boost-tree.Rmd details
#'
#' @note For models created using the spark engine, there are
#'  several differences to consider. First, only the formula
#'  interface to via `fit()` is available; using `fit_xy()` will
#'  generate an error. Second, the predictions will always be in a
#'  spark table format. The names will be the same as documented but
#'  without the dots. Third, there is no equivalent to factor
#'  columns in spark tables so class predictions are returned as
#'  character columns. Fourth, to retain the model object for a new
#'  R session (via `save()`), the `model$fit` element of the `parsnip`
#'  object should be serialized via `ml_save(object$fit)` and
#'  separately saved to disk. In a new session, the object can be
#'  reloaded and reattached to the `parsnip` object.
#'
#' @importFrom purrr map_lgl
#' @seealso [fit()], [set_engine()], [update()]
#' @examples
#' show_engines("boost_tree")
#'
#' boost_tree(mode = "classification", trees = 20)
#' # Parameters can be represented by a placeholder:
#' boost_tree(mode = "regression", mtry = varying())
#' @export

boost_tree <-
  function(mode = "unknown",
           engine = "xgboost",
           mtry = NULL, trees = NULL, min_n = NULL,
           tree_depth = NULL, learn_rate = NULL,
           loss_reduction = NULL,
           sample_size = NULL,
           stop_iter = NULL) {
    args <- list(
      mtry = enquo(mtry),
      trees = enquo(trees),
      min_n = enquo(min_n),
      tree_depth = enquo(tree_depth),
      learn_rate = enquo(learn_rate),
      loss_reduction = enquo(loss_reduction),
      sample_size = enquo(sample_size),
      stop_iter = enquo(stop_iter)
    )

    new_model_spec(
      "boost_tree",
      args,
      eng_args = NULL,
      mode,
      method = NULL,
      engine = engine
    )
  }

#' @export
print.boost_tree <- function(x, ...) {
  cat("Boosted Tree Model Specification (", x$mode, ")\n\n", sep = "")
  model_printer(x, ...)

  if (!is.null(x$method$fit$args)) {
    cat("Model fit template:\n")
    print(show_call(x))
  }
  invisible(x)
}

# ------------------------------------------------------------------------------

#' @method update boost_tree
#' @rdname parsnip_update
#' @export
update.boost_tree <-
  function(object,
           parameters = NULL,
           mtry = NULL, trees = NULL, min_n = NULL,
           tree_depth = NULL, learn_rate = NULL,
           loss_reduction = NULL, sample_size = NULL,
           stop_iter = NULL,
           fresh = FALSE, ...) {

    eng_args <- update_engine_parameters(object$eng_args, ...)

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
      stop_iter = enquo(stop_iter)
    )

    args <- update_main_parameters(args, parameters)

    # TODO make these blocks into a function and document well
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
      "boost_tree",
      args = object$args,
      eng_args = object$eng_args,
      mode = object$mode,
      method = NULL,
      engine = object$engine
    )
  }

# ------------------------------------------------------------------------------

#' @export
translate.boost_tree <- function(x, engine = x$engine, ...) {
  if (is.null(engine)) {
    message("Used `engine = 'xgboost'` for translation.")
    engine <- "xgboost"
  }
  x <- translate.default(x, engine, ...)

  ## -----------------------------------------------------------------------------

  arg_vals <- x$method$fit$args

  if (engine == "spark") {
    if (x$mode == "unknown") {
      rlang::abort(
        glue::glue(
          "For spark boosted trees models, the mode cannot be 'unknown' ",
          "if the specification is to be translated."
        )
      )
    } else {
      arg_vals$type <- x$mode
    }
  }

  ## -----------------------------------------------------------------------------
  # Protect some arguments based on data dimensions

  # min_n parameters
  if (any(names(arg_vals) == "min_instances_per_node")) {
    arg_vals$min_instances_per_node <-
      rlang::call2("min_rows", rlang::eval_tidy(arg_vals$min_instances_per_node), expr(x))
  }

  ## -----------------------------------------------------------------------------

  x$method$fit$args <- arg_vals

  x
}

# ------------------------------------------------------------------------------

check_args.boost_tree <- function(object) {

  args <- lapply(object$args, rlang::eval_tidy)

  if (is.numeric(args$trees) && args$trees < 0) {
    rlang::abort("`trees` should be >= 1.")
  }
  if (is.numeric(args$sample_size) && (args$sample_size < 0 | args$sample_size > 1)) {
    rlang::abort("`sample_size` should be within [0,1].")
  }
  if (is.numeric(args$tree_depth) && args$tree_depth < 0) {
    rlang::abort("`tree_depth` should be >= 1.")
  }
  if (is.numeric(args$min_n) && args$min_n < 0) {
    rlang::abort("`min_n` should be >= 1.")
  }

  invisible(object)
}

# xgboost helpers --------------------------------------------------------------

#' Boosted trees via xgboost
#'
#' `xgb_train` is a wrapper for `xgboost` tree-based models where all of the
#'  model arguments are in the main function.
#'
#' @param x A data frame or matrix of predictors
#' @param y A vector (factor or numeric) or matrix (numeric) of outcome data.
#' @param max_depth An integer for the maximum depth of the tree.
#' @param nrounds An integer for the number of boosting iterations.
#' @param eta A numeric value between zero and one to control the learning rate.
#' @param colsample_bytree Subsampling proportion of columns for each tree.
#' See the `counts` argument below. The default uses all columns.
#' @param colsample_bynode Subsampling proportion of columns for each node
#' within each tree. See the `counts` argument below. The default uses all
#' columns.
#' @param min_child_weight A numeric value for the minimum sum of instance
#'  weights needed in a child to continue to split.
#' @param gamma A number for the minimum loss reduction required to make a
#'  further partition on a leaf node of the tree
#' @param subsample Subsampling proportion of rows. By default, all of the
#' training data are used.
#' @param validation The _proportion_ of the data that are used for performance
#' assessment and potential early stopping.
#' @param early_stop An integer or `NULL`. If not `NULL`, it is the number of
#' training iterations without improvement before stopping. If `validation` is
#' used, performance is base on the validation set; otherwise, the training set
#' is used.
#' @param counts A logical. If `FALSE`, `colsample_bynode` and
#' `colsample_bytree` are both assumed to be _proportions_ of the proportion of
#' columns affects (instead of counts).
#' @param objective A single string (or NULL) that defines the loss function that
#' `xgboost` uses to create trees. See [xgboost::xgb.train()] for options. If left
#' NULL, an appropriate loss function is chosen.
#' @param event_level For binary classification, this is a single string of either
#' `"first"` or `"second"` to pass along describing which level of the outcome
#' should be considered the "event".
#' @param ... Other options to pass to `xgb.train`.
#' @return A fitted `xgboost` object.
#' @keywords internal
#' @export
xgb_train <- function(
  x, y,
  max_depth = 6, nrounds = 15, eta  = 0.3, colsample_bynode = NULL,
  colsample_bytree = NULL, min_child_weight = 1, gamma = 0, subsample = 1,
  validation = 0, early_stop = NULL, objective = NULL, counts = TRUE,
  event_level = c("first", "second"), ...) {

  event_level <- rlang::arg_match(event_level, c("first", "second"))
  others <- list(...)

  num_class <- length(levels(y))

  if (!is.numeric(validation) || validation < 0 || validation >= 1) {
    rlang::abort("`validation` should be on [0, 1).")
  }

  if (!is.null(early_stop)) {
    if (early_stop <= 1) {
      rlang::abort(paste0("`early_stop` should be on [2, ",  nrounds, ")."))
    } else if (early_stop >= nrounds) {
      early_stop <- nrounds - 1
      rlang::warn(paste0("`early_stop` was reduced to ", early_stop, "."))
    }
  }

  if (is.null(objective)) {
    if (is.numeric(y)) {
      objective <- "reg:squarederror"
    } else {
      if (num_class == 2) {
        objective <- "binary:logistic"
      } else {
        objective <- "multi:softprob"
      }
    }
  }

  n <- nrow(x)
  p <- ncol(x)

  x <- as_xgb_data(x, y, validation, event_level)


  if (!is.numeric(subsample) || subsample < 0 || subsample > 1) {
    rlang::abort("`subsample` should be on [0, 1].")
  }

  # initialize
  if (is.null(colsample_bytree)) {
    colsample_bytree <- 1
  } else {
    colsample_bytree <- recalc_param(colsample_bytree, counts, p)
  }
  if (is.null(colsample_bynode)) {
    colsample_bynode <- 1
  } else {
    colsample_bynode <- recalc_param(colsample_bynode, counts, p)
  }

  if (min_child_weight > n) {
    msg <- paste0(min_child_weight, " samples were requested but there were ",
                  n, " rows in the data. ", n, " will be used.")
    rlang::warn(msg)
    min_child_weight <- min(min_child_weight, n)
  }

  arg_list <- list(
    eta = eta,
    max_depth = max_depth,
    gamma = gamma,
    colsample_bytree = colsample_bytree,
    colsample_bynode = colsample_bynode,
    min_child_weight = min(min_child_weight, n),
    subsample = subsample,
    objective = objective
  )

  main_args <- list(
    data = quote(x$data),
    watchlist = quote(x$watchlist),
    params = arg_list,
    nrounds = nrounds,
    early_stopping_rounds = early_stop
  )
  if (!is.null(num_class) && num_class > 2) {
    main_args$num_class <- num_class
  }

  call <- make_call(fun = "xgb.train", ns = "xgboost", main_args)

  # override or add some other args

  others <-
    others[!(names(others) %in% c("data", "weights", "nrounds", "num_class", names(arg_list)))]
  if (!(any(names(others) == "verbose"))) {
    others$verbose <- 0
  }
  if (length(others) > 0) {
    call <- rlang::call_modify(call, !!!others)
  }

  eval_tidy(call, env = current_env())
}

recalc_param <- function(x, counts, denom) {
  nm <- as.character(match.call()$x)
  if (is.null(x)) {
    x <- 1
  } else {
    if (counts) {
      maybe_proportion(x, nm)
      x <- min(denom, x)/denom
    }
  }
  x
}

maybe_proportion <- function(x, nm) {
  if (x < 1) {
    msg <- paste0(
      "The option `counts = TRUE` was used but parameter `", nm,
      "` was given as ", signif(x, 3), ". Please use a value >= 1 or use ",
      "`counts = FALSE`."
    )
    rlang::abort(msg)
  }
}

#' @importFrom stats binomial
xgb_pred <- function(object, newdata, ...) {
  if (!inherits(newdata, "xgb.DMatrix")) {
    newdata <- maybe_matrix(newdata)
    newdata <- xgboost::xgb.DMatrix(data = newdata, missing = NA)
  }

  res <- predict(object, newdata, ...)

  x <- switch(
    object$params$objective,
    "binary:logitraw" = stats::binomial()$linkinv(res),
    "multi:softprob" = matrix(res, ncol = object$params$num_class, byrow = TRUE),
    res)

  x
}


as_xgb_data <- function(x, y, validation = 0, event_level = "first", ...) {
  lvls <- levels(y)
  n <- nrow(x)

  if (is.data.frame(x)) {
    x <- as.matrix(x)
  }

  if (is.factor(y)) {
    if (length(lvls) < 3) {
      if (event_level == "first") {
        y <- -as.numeric(y) + 2
      } else {
        y <- as.numeric(y) - 1
      }
    } else {
      if (event_level == "second") rlang::warn("`event_level` can only be set for binary variables.")
      y <- as.numeric(y) - 1
    }
  }

  if (!inherits(x, "xgb.DMatrix")) {
    if (validation > 0) {
      m <- floor(n * (1 - validation)) + 1
      trn_index <- sample(1:n, size = max(m, 2))
      wlist <-
        list(validation = xgboost::xgb.DMatrix(x[-trn_index, ], label = y[-trn_index], missing = NA))
      dat <- xgboost::xgb.DMatrix(x[trn_index, ], label = y[trn_index], missing = NA)

    } else {
      dat <- xgboost::xgb.DMatrix(x, label = y, missing = NA)
      wlist <- list(training = dat)
    }
  } else {
    dat <- xgboost::setinfo(x, "label", y)
    wlist <- list(training = dat)
  }

  list(data = dat, watchlist = wlist)
}

get_event_level <- function(model_spec){
  if ("event_level" %in% names(model_spec$eng_args)) {
    event_level <- get_expr(model_spec$eng_args$event_level)
  } else {
    # "first" is the default for as_xgb_data() and xgb_train()
    event_level <- "first"
  }
  event_level
}

#' @importFrom purrr map_df
#' @export
#' @rdname multi_predict
#' @param trees An integer vector for the number of trees in the ensemble.
multi_predict._xgb.Booster <-
  function(object, new_data, type = NULL, trees = NULL, ...) {
    if (any(names(enquos(...)) == "newdata")) {
      rlang::abort("Did you mean to use `new_data` instead of `newdata`?")
    }

    if (is.null(trees)) {
      trees <- object$fit$nIter
    }
    trees <- sort(trees)

    if (is.null(type)) {
      if (object$spec$mode == "classification")
        type <- "class"
      else
        type <- "numeric"
    }

    res <- map_df(trees, xgb_by_tree, object = object, new_data = new_data,
                  type = type, ...)
    res <- arrange(res, .row, trees)
    res <- split(res[, -1], res$.row)
    names(res) <- NULL
    tibble(.pred = res)
  }

xgb_by_tree <- function(tree, object, new_data, type, ...) {
  pred <- xgb_pred(object$fit, newdata = new_data, ntreelimit = tree)

  # switch based on prediction type
  if (object$spec$mode == "regression") {
    pred <- tibble(.pred = pred)
    nms <- names(pred)
  } else {
    if (type == "class") {
      pred <- object$spec$method$pred$class$post(pred, object)
      pred <- tibble(.pred_class = factor(pred, levels = object$lvl))
    } else {
      pred <- object$spec$method$pred$prob$post(pred, object)
      pred <- as_tibble(pred)
      names(pred) <- paste0(".pred_", names(pred))
    }
    nms <- names(pred)
  }
  pred[["trees"]] <- tree
  pred[[".row"]] <- 1:nrow(new_data)
  pred[, c(".row", "trees", nms)]
}

# C5.0 helpers -----------------------------------------------------------------

#' Boosted trees via C5.0
#'
#' `C5.0_train` is a wrapper for the `C5.0()` function in the
#' \pkg{C50} package that fits tree-based models
#'  where all of the model arguments are in the main function.
#'
#' @param x A data frame or matrix of predictors.
#' @param y A factor vector with 2 or more levels
#' @param trials An integer specifying the number of boosting
#'  iterations. A value of one indicates that a single model is
#'  used.
#' @param weights An optional numeric vector of case weights. Note
#'  that the data used for the case weights will not be used as a
#'  splitting variable in the model (see
#'  \url{https://www.rulequest.com/see5-info.html} for
#'  Quinlan's notes on case weights).
#' @param minCases An integer for the smallest number of samples
#'  that must be put in at least two of the splits.
#' @param sample A value between (0, .999) that specifies the
#'  random proportion of the data should be used to train the model.
#'  By default, all the samples are used for model training. Samples
#'  not used for training are used to evaluate the accuracy of the
#'  model in the printed output.
#' @param ... Other arguments to pass.
#' @return A fitted C5.0 model.
#' @keywords internal
#' @export
C5.0_train <-
  function(x, y, weights = NULL, trials = 15, minCases = 2, sample = 0, ...) {
    other_args <- list(...)
    protect_ctrl <- c("minCases", "sample")
    protect_fit <- "trials"
    f_names <- names(formals(getFromNamespace("C5.0.default", "C50")))
    c_names <- names(formals(getFromNamespace("C5.0Control", "C50")))
    other_args <- other_args[!(other_args %in% c(protect_ctrl, protect_fit))]
    ctrl_args <- other_args[names(other_args) %in% c_names]
    fit_args <- other_args[names(other_args) %in% f_names]

    n <- nrow(x)
    if (n == 0) {
      rlang::abort("There are zero rows in the predictor set.")
    }


    ctrl <- call2("C5.0Control", .ns = "C50")
    if (minCases > n) {
      msg <- paste0(minCases, " samples were requested but there were ",
                    n, " rows in the data. ", n, " will be used.")
      rlang::warn(msg)
      minCases <- n
    }
    ctrl$minCases <- minCases

    ctrl$sample <- sample
    ctrl <- rlang::call_modify(ctrl, !!!ctrl_args)

    fit_call <- call2("C5.0", .ns = "C50")
    fit_call$x <- expr(x)
    fit_call$y <- expr(y)
    fit_call$trials <- trials
    fit_call$control <- ctrl
    if (!is.null(weights)) {
      fit_call$weights <- quote(weights)
    }
    fit_call <- rlang::call_modify(fit_call, !!!fit_args)

    eval_tidy(fit_call)
  }

#' @export
#' @rdname multi_predict
multi_predict._C5.0 <-
  function(object, new_data, type = NULL, trees = NULL, ...) {
    if (any(names(enquos(...)) == "newdata"))
      rlang::abort("Did you mean to use `new_data` instead of `newdata`?")

    if (is.null(trees))
      trees <- min(object$fit$trials)
    trees <- sort(trees)

    if (is.null(type))
      type <- "class"

    res <-
      map_df(trees, C50_by_tree, object = object,
             new_data = new_data, type = type, ...)
    res <- arrange(res, .row, trees)
    res <- split(res[, -1], res$.row)
    names(res) <- NULL
    tibble(.pred = res)
  }

C50_by_tree <- function(tree, object, new_data, type, ...) {
  pred <- predict(object$fit, newdata = new_data, trials = tree, type = type)

  # switch based on prediction type
  if (type == "class") {
    pred <- tibble(.pred_class = factor(pred, levels = object$lvl))
  } else {
    pred <- as_tibble(pred)
    names(pred) <- paste0(".pred_", names(pred))
  }
  nms <- names(pred)
  pred[["trees"]] <- tree
  pred[[".row"]] <- 1:nrow(new_data)
  pred[, c(".row", "trees", nms)]
}


