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
#' @seealso [fit()], [set_engine()]
#' @examples
#' show_engines("boost_tree")
#'
#' boost_tree(mode = "classification", trees = 20)
#' # Parameters can be represented by a placeholder:
#' boost_tree(mode = "regression", mtry = varying())
#' @export

boost_tree <-
  function(mode = "unknown",
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
      engine = NULL
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

#' @export
#' @param object A boosted tree model specification.
#' @param parameters A 1-row tibble or named list with _main_
#'  parameters to update. If the individual arguments are used,
#'  these will supersede the values in `parameters`. Also, using
#'  engine arguments in this object will result in an error.
#' @param ... Not used for `update()`.
#' @param fresh A logical for whether the arguments should be
#'  modified in-place of or replaced wholesale.
#' @return An updated model specification.
#' @examples
#' model <- boost_tree(mtry = 10, min_n = 3)
#' model
#' update(model, mtry = 1)
#' update(model, mtry = 1, fresh = TRUE)
#'
#' param_values <- tibble::tibble(mtry = 10, tree_depth = 5)
#'
#' model %>% update(param_values)
#' model %>% update(param_values, mtry = 3)
#'
#' param_values$verbose <- 0
#' # Fails due to engine argument
#' # model %>% update(param_values)
#' @method update boost_tree
#' @rdname boost_tree
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

  if (engine == "spark") {
    if (x$mode == "unknown") {
      rlang::abort(
        glue::glue(
          "For spark boosted trees models, the mode cannot be 'unknown' ",
          "if the specification is to be translated."
        )
      )
    } else {
      x$method$fit$args$type <- x$mode
    }
  }
  x
}

# ------------------------------------------------------------------------------

check_args.boost_tree <- function(object) {

  args <- lapply(object$args, rlang::eval_tidy)

  if (is.numeric(args$trees) && args$trees < 0)
    rlang::abort("`trees` should be >= 1.")
  if (is.numeric(args$sample_size) && (args$sample_size < 0 | args$sample_size > 1))
    rlang::abort("`sample_size` should be within [0,1].")
  if (is.numeric(args$tree_depth) && args$tree_depth < 0)
    rlang::abort("`tree_depth` should be >= 1.")
  if (is.numeric(args$min_n) && args$min_n < 0)
    rlang::abort("`min_n` should be >= 1.")

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
#' @param colsample_bytree Subsampling proportion of columns.
#' @param min_child_weight A numeric value for the minimum sum of instance
#'  weights needed in a child to continue to split.
#' @param gamma A number for the minimum loss reduction required to make a
#'  further partition on a leaf node of the tree
#' @param subsample Subsampling proportion of rows.
#' @param validation A positive number. If on `[0, 1)` the value, `validation`
#' is a random proportion of data in `x` and `y` that are used for performance
#' assessment and potential early stopping. If 1 or greater, it is the _number_
#' of training set samples use for these purposes.
#' @param early_stop An integer or `NULL`. If not `NULL`, it is the number of
#' training iterations without improvement before stopping. If `validation` is
#' used, performance is base on the validation set; otherwise, the training set
#' is used.
#' @param ... Other options to pass to `xgb.train`.
#' @return A fitted `xgboost` object.
#' @keywords internal
#' @export
xgb_train <- function(
  x, y,
  max_depth = 6, nrounds = 15, eta  = 0.3, colsample_bytree = 1,
  min_child_weight = 1, gamma = 0, subsample = 1, validation = 0,
  early_stop = NULL, ...) {

  if (length(levels(y)) > 2) {
    num_class <- length(levels(y))
  }  else {
    num_class <- NULL
  }
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


  if (is.numeric(y)) {
    loss <- "reg:squarederror"
  } else {
    lvl <- levels(y)
    y <- as.numeric(y) - 1
    if (length(lvl) == 2) {
      loss <- "binary:logistic"
    } else {
      loss <- "multi:softprob"
    }
  }

  if (is.data.frame(x)) {
    x <- as.matrix(x) # maybe use model.matrix here?
  }

  n <- nrow(x)
  p <- ncol(x)

  if (!inherits(x, "xgb.DMatrix")) {
    if (validation > 0) {
      trn_index <- sample(1:n, size = floor(n * validation) + 1)
      wlist <-
        list(validation = xgboost::xgb.DMatrix(x[-trn_index, ], label = y[-trn_index], missing = NA))
      x <- xgboost::xgb.DMatrix(x[trn_index, ], label = y[trn_index], missing = NA)

    } else {
      x <- xgboost::xgb.DMatrix(x, label = y, missing = NA)
      wlist <- list(training = x)
    }
  } else {
    xgboost::setinfo(x, "label", y)
  }

  # translate `subsample` and `colsample_bytree` to be on (0, 1] if not
  if (subsample > 1) {
    subsample <- subsample/n
  }
  if (subsample > 1) {
    subsample <- 1
  }

  if (colsample_bytree > 1) {
    colsample_bytree <- colsample_bytree/p
  }
  if (colsample_bytree > 1) {
    colsample_bytree <- 1
  }

  arg_list <- list(
    eta = eta,
    max_depth = max_depth,
    gamma = gamma,
    colsample_bytree = colsample_bytree,
    min_child_weight = min_child_weight,
    subsample = subsample
  )

  # eval if contains expressions?

  main_args <- list(
    data = quote(x),
    watchlist = quote(wlist),
    params = arg_list,
    nrounds = nrounds,
    objective = loss,
    early_stopping_rounds = early_stop
  )
  if (!is.null(num_class)) {
    main_args$num_class <- num_class
  }

  call <- make_call(fun = "xgb.train", ns = "xgboost", main_args)

  # override or add some other args
  others <- list(...)
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

#' @importFrom stats binomial
xgb_pred <- function(object, newdata, ...) {
  if (!inherits(newdata, "xgb.DMatrix")) {
    newdata <- as.matrix(newdata)
    newdata <- xgboost::xgb.DMatrix(data = newdata, missing = NA)
  }

  res <- predict(object, newdata, ...)

  x = switch(
    object$params$objective,
    "reg:squarederror" = , "reg:logistic" = , "binary:logistic" = res,
    "binary:logitraw" = stats::binomial()$linkinv(res),
    "multi:softprob" = matrix(res, ncol = object$params$num_class, byrow = TRUE),
    res
  )
  x
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
#'  \url{http://www.rulequest.com/see5-win.html} for
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

    ctrl <- call2("C5.0Control", .ns = "C50")
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


