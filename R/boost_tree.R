# Prototype parsnip code for boosted trees

#' Boosted trees
#'
#' @description
#'
#' `boost_tree()` defines a model that creates a series of decision trees
#' forming an ensemble. Each tree depends on the results of previous trees.
#' All trees in the ensemble are combined to produce a final prediction. This
#' function can fit classification, regression, and censored regression models.
#'
#' \Sexpr[stage=render,results=rd]{parsnip:::make_engine_list("boost_tree")}
#'
#' More information on how \pkg{parsnip} is used for modeling is at
#' \url{https://www.tidymodels.org/}.
#'
#' @param mode A single character string for the prediction outcome mode.
#'  Possible values for this model are "unknown", "regression",
#'  "classification", or "censored regression".
#' @param engine A single character string specifying what computational engine
#'  to use for fitting.
#' @param mtry A number for the number (or proportion) of predictors that will
#'  be randomly sampled at each split when creating the tree models
#' (specific engines only).
#' @param trees An integer for the number of trees contained in
#'  the ensemble.
#' @param min_n An integer for the minimum number of data points
#'  in a node that is required for the node to be split further.
#' @param tree_depth An integer for the maximum depth of the tree (i.e. number
#'  of splits) (specific engines only).
#' @param learn_rate A number for the rate at which the boosting algorithm adapts
#'   from iteration-to-iteration (specific engines only). This is sometimes referred to
#'   as the shrinkage parameter.
#' @param loss_reduction A number for the reduction in the loss function required
#'   to split further (specific engines only).
#' @param sample_size A number for the number (or proportion) of data that is
#'  exposed to the fitting routine. For `xgboost`, the sampling is done at
#'  each iteration while `C5.0` samples once during training.
#' @param stop_iter The number of iterations without improvement before
#'   stopping (specific engines only).
#'
#' @templateVar modeltype boost_tree
#' @template spec-details
#'
#' @template spec-references
#'
#' @seealso \Sexpr[stage=render,results=rd]{parsnip:::make_seealso_list("boost_tree")},
#' [xgb_train()], [C5.0_train()]
#'
#' @examplesIf !parsnip:::is_cran_check()
#' show_engines("boost_tree")
#'
#' boost_tree(mode = "classification", trees = 20)
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
      mode = mode,
      user_specified_mode = !missing(mode),
      method = NULL,
      engine = engine,
      user_specified_engine = !missing(engine)
    )
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

    update_spec(
      object = object,
      parameters = parameters,
      args_enquo_list = args,
      fresh = fresh,
      cls = "boost_tree",
      ...
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
      cli::cli_abort(
        "For spark boosted tree models, the mode cannot be {.val unknown}
         if the specification is to be translated."
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

#' @export
check_args.boost_tree <- function(object, call = rlang::caller_env()) {

  args <- lapply(object$args, rlang::eval_tidy)

  check_number_whole(args$trees, min = 0, allow_null = TRUE, call = call, arg = "trees")
  check_number_decimal(args$sample_size, min = 0, max = 1, allow_null = TRUE, call = call, arg = "sample_size")
  check_number_whole(args$tree_depth, min = 0, allow_null = TRUE, call = call, arg = "tree_depth")
  check_number_whole(args$min_n, min = 0, allow_null = TRUE, call = call, arg = "min_n")

  invisible(object)
}


# xgboost helpers --------------------------------------------------------------

#' Boosted trees via xgboost
#'
#' `xgb_train()` and `xgb_predict()` are wrappers for `xgboost` tree-based
#' models where all of the model arguments are in the main function.
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
#' @param event_level For binary classification, this is a single string of either
#' `"first"` or `"second"` to pass along describing which level of the outcome
#' should be considered the "event".
#' @param ... Other options to pass to `xgb.train()` or xgboost's method for `predict()`.
#' @return A fitted `xgboost` object.
#' @keywords internal
#' @export
xgb_train <- function(
    x, y, weights = NULL,
    max_depth = 6, nrounds = 15, eta  = 0.3, colsample_bynode = NULL,
    colsample_bytree = NULL, min_child_weight = 1, gamma = 0, subsample = 1,
    validation = 0, early_stop = NULL, counts = TRUE,
    event_level = c("first", "second"), ...) {

  event_level <- rlang::arg_match(event_level, c("first", "second"))
  others <- list(...)

  num_class <- length(levels(y))

  if (!is.numeric(validation) || validation < 0 || validation >= 1) {
    cli::cli_abort("{.arg validation} should be on [0, 1).")
  }

  if (!is.null(early_stop)) {
    if (early_stop <= 1) {
      cli::cli_abort("{.arg early_stop} should be on [2, {nrounds}).")
    } else if (early_stop >= nrounds) {
      early_stop <- nrounds - 1
      cli::cli_warn("{.arg early_stop} was reduced to {early_stop}.")
    }
  }

  n <- nrow(x)
  p <- ncol(x)

  x <-
    as_xgb_data(x, y,
                validation = validation,
                event_level = event_level,
                weights = weights)


  if (!is.numeric(subsample) || subsample < 0 || subsample > 1) {
    cli::cli_abort("{.arg subsample} should be on [0, 1].")
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
    cli::cli_warn(
      c(
       "!" = "{min_child_weight} samples were requested but there were {n} rows
              in the data.",
       "i" = "{n} will be used."
      )
    )
    min_child_weight <- min(min_child_weight, n)
  }

  arg_list <- list(
    eta = eta,
    max_depth = max_depth,
    gamma = gamma,
    colsample_bytree = colsample_bytree,
    colsample_bynode = colsample_bynode,
    min_child_weight = min(min_child_weight, n),
    subsample = subsample
  )

  others <- process_others(others, arg_list)

  main_args <- c(
      list(
        data = quote(x$data),
        watchlist = quote(x$watchlist),
        params = arg_list,
        nrounds = nrounds,
        early_stopping_rounds = early_stop
      ),
      others
  )

  if (is.null(main_args$objective)) {
    if (is.numeric(y)) {
      main_args$objective <- "reg:squarederror"
    } else {
      if (num_class == 2) {
        main_args$objective <- "binary:logistic"
      } else {
        main_args$objective <- "multi:softprob"
      }
    }
  }

  if (!is.null(num_class) && num_class > 2) {
    main_args$num_class <- num_class
  }

  call <- make_call(fun = "xgb.train", ns = "xgboost", main_args)

  eval_tidy(call, env = current_env())
}

process_others <- function(others, arg_list) {
  guarded <- c("data", "weights", "num_class", "watchlist")
  guarded_supplied <- names(others)[names(others) %in% guarded]

  if (length(guarded_supplied) > 0) {
    cli::cli_warn(
      c(
        "!" = "{cli::qty(guarded_supplied)}The argument{?s} {.arg {guarded_supplied}} \
               {?is/are} guarded by parsnip and will not be passed to {.fun xgb.train}."
      ),
      class = "xgboost_guarded_warning"
    )
  }

  others <-
    others[!(names(others) %in% guarded)]

  if (!is.null(others$params)) {
    cli::cli_warn(
      c(
        "!" = "Please supply elements of the `params` list argument as main arguments \
               to `set_engine()` rather than as part of `params`.",
        "i" = "See `?details_boost_tree_xgboost` for more information."
      ),
      class = "xgboost_params_warning"
    )

    params <- others$params[!names(others$params) %in% names(arg_list)]
    others <- c(others[names(others) != "params"], params)
  }

  if (!(any(names(others) == "verbose"))) {
    others$verbose <- 0
  }

  others
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
    cli::cli_abort(
      c(
        "The option `counts = TRUE` was used but {.arg {nm}} was given
         as {signif(x, 3)}.",
        "i" = "Please use a value >= 1 or use {.code counts = FALSE}."
      ),
      call = call2("xgb_train")
    )
  }
}

#' @rdname xgb_train
#' @param new_data A rectangular data object, such as a data frame.
#' @keywords internal
#' @export
xgb_predict <- function(object, new_data, ...) {
  if (!inherits(new_data, "xgb.DMatrix")) {
    new_data <- maybe_matrix(new_data)
    new_data <- xgboost::xgb.DMatrix(data = new_data, missing = NA)
  }

  res <- predict(object, new_data, ...)

  x <- switch(
    object$params$objective %||% 3L,
    "binary:logitraw" = stats::binomial()$linkinv(res),
    "multi:softprob" = matrix(res, ncol = object$params$num_class, byrow = TRUE),
    res)

  x
}


as_xgb_data <- function(x, y, validation = 0, weights = NULL, event_level = "first", ...) {
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
      if (event_level == "second") {
        cli::cli_warn("{.arg event_level} can only be set for binary outcomes.")
      }
      y <- as.numeric(y) - 1
    }
  }

  if (!inherits(x, "xgb.DMatrix")) {
    if (validation > 0) {
      # Split data
      m <- floor(n * (1 - validation)) + 1
      trn_index <- sample(seq_len(n), size = max(m, 2))
      val_data <- xgboost::xgb.DMatrix(
        data = x[-trn_index, , drop = FALSE],
        label = y[-trn_index],
        missing = NA
      )
      watch_list <- list(validation = val_data)

      info_list <- list(label = y[trn_index])
      if (!is.null(weights)) {
        info_list$weight <- weights[trn_index]
      }
      dat <- xgboost::xgb.DMatrix(
        data = x[trn_index, , drop = FALSE],
        missing = NA,
        info = info_list
      )

    } else {
      info_list <- list(label = y)
      if (!is.null(weights)) {
        info_list$weight <- weights
      }
      dat <- xgboost::xgb.DMatrix(x, missing = NA, info = info_list)
      watch_list <- list(training = dat)
    }
  } else {
    dat <- xgboost::setinfo(x, "label", y)
    if (!is.null(weights)) {
      dat <- xgboost::setinfo(x, "weight", weights)
    }
    watch_list <- list(training = dat)
  }

  list(data = dat, watchlist = watch_list)
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


#' @export
#' @rdname multi_predict
#' @param trees An integer vector for the number of trees in the ensemble.
multi_predict._xgb.Booster <-
  function(object, new_data, type = NULL, trees = NULL, ...) {
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

    res <-
      map(trees, xgb_by_tree, object = object, new_data = new_data,
          type = type, ...) |>
      purrr::list_rbind()
    res <- arrange(res, .row, trees)
    res <- split(res[, -1], res$.row)
    names(res) <- NULL
    tibble(.pred = res)
  }

xgb_by_tree <- function(tree, object, new_data, type, ...) {
  pred <- xgb_predict(
    object$fit,
    new_data = new_data,
    iterationrange = c(1, tree + 1),
    ntreelimit = NULL
  )

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
  pred[[".row"]] <- seq_len(nrow(new_data))
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
#'  `https://www.rulequest.com/see5-info.html` for
#'  Quinlan's notes on case weights).
#' @param minCases An integer for the smallest number of samples
#'  that must be put in at least two of the splits.
#' @param sample A value between (0, .999) that specifies the
#'  random proportion of the data should be used to train the model.
#'  By default, all the samples are used for model training. Samples
#'  not used for training are used to evaluate the accuracy of the
#'  model in the printed output. A value of zero means that all the training
#'  data are used.
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
      cli::cli_abort("There are zero rows in the predictor set.")
    }


    ctrl <- call2("C5.0Control", .ns = "C50")
    if (minCases > n) {

      cli::cli_warn(
        c(
          "!" = "{minCases} samples were requested but there were {n} rows in the data.",
          "i" = "{n} will be used."
        )
      )
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
    if (is.null(trees))
      trees <- min(object$fit$trials)
    trees <- sort(trees)

    if (is.null(type))
      type <- "class"

    res <-
      map(trees, C50_by_tree, object = object,
          new_data = new_data, type = type, ...) |>
      purrr::list_rbind()
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
  pred[[".row"]] <- seq_len(nrow(new_data))
  pred[, c(".row", "trees", nms)]
}


