# Prototype parsnip code for boosted trees

#' General Interface for Boosted Trees
#'
#' `boost_tree` is a way to generate a _specification_ of a model
#'  before fitting and allows the model to be created using
#'  different packages in R or via Spark. The main arguments for the
#'  model are:
#' \itemize{
#'   \item \code{mtry}: The number of predictors that will be
#'   randomly sampled at each split when creating the tree models.
#'   \item \code{trees}: The number of trees contained in the ensemble.
#'   \item \code{min_n}: The minimum number of data points in a node
#'   that are required for the node to be split further.
#'   \item \code{tree_depth}: The maximum depth of the tree (i.e. number of
#'  splits).
#'   \item \code{learn_rate}: The rate at which the boosting algorithm adapts
#'   from iteration-to-iteration.
#'   \item \code{loss_reduction}: The reduction in the loss function required
#'   to split further.
#'   \item \code{sample_size}: The amount of data exposed to the fitting routine.
#' }
#' These arguments are converted to their specific names at the
#'  time that the model is fit. Other options and argument can be
#'  set using the  `...` slot. If left to their defaults
#'  here (`NULL`), the values are taken from the underlying model
#'  functions.  If parameters need to be modified, `update` can be used
#'  in lieu of recreating the object from scratch.
#'
#' @param mode A single character string for the type of model.
#'  Possible values for this model are "unknown", "regression", or
#'  "classification".
#' @param mtry An number for the number (or proportion) of predictors that will
#'  be randomly sampled at each split when creating the tree models (`xgboost`
#'  only).
#' @param trees An integer for the number of trees contained in
#'  the ensemble.
#' @param min_n An integer for the minimum number of data points
#'  in a node that are required for the node to be split further.
#' @param tree_depth An integer for the maximum deopth of the tree (i.e. number
#'  of splits) (`xgboost` only).
#' @param learn_rate A number for the rate at which the boosting algorithm adapts
#'   from iteration-to-iteration (`xgboost` only).
#' @param loss_reduction A number for the reduction in the loss function required
#'   to split further  (`xgboost` only).
#' @param sample_size An number for the number (or proportion) of data that is
#'  exposed to the fitting routine. For `xgboost`, the sampling is done at at
#'  each iteration while `C5.0` samples once during traning.
#' @param ... Other arguments to pass to the specific engine's
#'  model fit function (see the Engine Details section below). This
#'  should not include arguments defined by the main parameters to
#'  this function. For the `update` function, the ellipses can
#'  contain the primary arguments or any others.
#' @details
#' The data given to the function are not saved and are only used
#'  to determine the _mode_ of the model. For `boost_tree`, the
#'  possible modes are "regression" and "classification".
#'
#' The model can be created using the `fit()` function using the
#'  following _engines_:
#' \itemize{
#' \item \pkg{R}:  `"xgboost"`, `"C5.0"`
#' \item \pkg{Spark}: `"spark"`
#' }
#'
#' Main parameter arguments (and those in `...`) can avoid
#'  evaluation until the underlying function is executed by wrapping the
#'  argument in [rlang::expr()] (e.g. `mtry = expr(floor(sqrt(p)))`).
#'
#'
#' @section Engine Details:
#'
#' Engines may have pre-set default arguments when executing the
#'  model fit call. These can be changed by using the `...`
#'  argument to pass in the preferred values. For this type of
#'  model, the template of the fit calls are:
#'
#' \pkg{xgboost} classification
#'
#' \Sexpr[results=rd]{parsnip:::show_fit(parsnip:::boost_tree(mode = "classification"), "xgboost")}
#'
#' \pkg{xgboost} regression
#'
#' \Sexpr[results=rd]{parsnip:::show_fit(parsnip:::boost_tree(mode = "regression"), "xgboost")}
#'
#' \pkg{C5.0} classification
#'
#' \Sexpr[results=rd]{parsnip:::show_fit(parsnip:::boost_tree(mode = "classification"), "C5.0")}
#'
#' \pkg{spark} classification
#'
#' \Sexpr[results=rd]{parsnip:::show_fit(parsnip:::boost_tree(mode = "classification"), "spark")}
#'
#' \pkg{spark} regression
#'
#' \Sexpr[results=rd]{parsnip:::show_fit(parsnip:::boost_tree(mode = "regression"), "spark")}
#'
#' @note For models created using the spark engine, there are
#'  several differences to consider. First, only the formula
#'  interface to via `fit` is available; using `fit_xy` will
#'  generate an error. Second, the predictions will always be in a
#'  spark table format. The names will be the same as documented but
#'  without the dots. Third, there is no equivalent to factor
#'  columns in spark tables so class predictions are returned as
#'  character columns. Fourth, to retain the model object for a new
#'  R session (via `save`), the `model$fit` element of the `parsnip`
#'  object should be serialized via `ml_save(object$fit)` and
#'  separately saved to disk. In a new session, the object can be
#'  reloaded and reattached to the `parsnip` object.
#'
#' @importFrom purrr map_lgl
#' @seealso [varying()], [fit()]
#' @examples
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
           ...) {

    others <- enquos(...)

    args <- list(
      mtry = enquo(mtry),
      trees = enquo(trees),
      min_n = enquo(min_n),
      tree_depth = enquo(tree_depth),
      learn_rate = enquo(learn_rate),
      loss_reduction = enquo(loss_reduction),
      sample_size = enquo(sample_size)
    )

    if (!(mode %in% boost_tree_modes))
      stop("`mode` should be one of: ",
           paste0("'", boost_tree_modes, "'", collapse = ", "),
           call. = FALSE)

    no_value <- !vapply(others, null_value, logical(1))
    others <- others[no_value]

    out <- list(args = args, others = others,
                mode = mode, method = NULL, engine = NULL)
    class(out) <- make_classes("boost_tree")
    out
  }

#' @export
print.boost_tree <- function(x, ...) {
  cat("Boosted Tree Model Specification (", x$mode, ")\n\n", sep = "")
  model_printer(x, ...)

  if(!is.null(x$method$fit$args)) {
    cat("Model fit template:\n")
    print(show_call(x))
  }
  invisible(x)
}

# ------------------------------------------------------------------------------

#' @export
#' @inheritParams boost_tree
#' @param object A boosted tree model specification.
#' @param fresh A logical for whether the arguments should be
#'  modified in-place of or replaced wholesale.
#' @return An updated model specification.
#' @examples
#' model <- boost_tree(mtry = 10, min_n = 3)
#' model
#' update(model, mtry = 1)
#' update(model, mtry = 1, fresh = TRUE)
#' @method update boost_tree
#' @rdname boost_tree
#' @export
update.boost_tree <-
  function(object,
           mtry = NULL, trees = NULL, min_n = NULL,
           tree_depth = NULL, learn_rate = NULL,
           loss_reduction = NULL, sample_size = NULL,
           fresh = FALSE,
           ...) {

    others <- enquos(...)

    args <- list(
      mtry = enquo(mtry),
      trees = enquo(trees),
      min_n = enquo(min_n),
      tree_depth = enquo(tree_depth),
      learn_rate = enquo(learn_rate),
      loss_reduction = enquo(loss_reduction),
      sample_size = enquo(sample_size)
    )

    # TODO make these blocks into a function and document well
    if (fresh) {
      object$args <- args
    } else {
      null_args <- map_lgl(args, null_value)
      if (any(null_args))
        args <- args[!null_args]
      if (length(args) > 0)
        object$args[names(args)] <- args
    }

    if (length(others) > 0) {
      if (fresh)
        object$others <- others
      else
        object$others[names(others)] <- others
    }

    object
  }

# ------------------------------------------------------------------------------

#' @export
translate.boost_tree <- function(x, engine, ...) {
  x <- translate.default(x, engine, ...)

  if (x$engine == "spark") {
    if (x$mode == "unknown")
      stop(
        "For spark boosted trees models, the mode cannot be 'unknown' ",
        "if the specification is to be translated.",
        call. = FALSE
      )
    else
      x$method$fit$args$type <- x$mode
  }
  x
}

# ------------------------------------------------------------------------------

check_args.boost_tree <- function(object) {

  args <- lapply(object$args, rlang::eval_tidy)

  if (is.numeric(args$trees) && args$trees < 0)
    stop("`trees` should be >= 1", call. = FALSE)
  if (is.numeric(args$sample_size) && (args$sample_size < 0 | args$sample_size > 1))
    stop("`sample_size` should be within [0,1]", call. = FALSE)
  if (is.numeric(args$tree_depth) && args$tree_depth < 0)
    stop("`tree_depth` should be >= 1", call. = FALSE)
  if (is.numeric(args$min_n) && args$min_n < 0)
    stop("`min_n` should be >= 1", call. = FALSE)

}

# xgboost helpers --------------------------------------------------------------

xgb_train <- function(
  x, y,
  max_depth = 6, nrounds = 15, eta  = 0.3, colsample_bytree = 1,
  min_child_weight = 1, gamma = 0, subsample = 1, ...) {

  num_class <- if (length(levels(y)) > 2) length(levels(y)) else NULL

  if (is.numeric(y)) {
    loss <- "reg:linear"
  } else {
    lvl <- levels(y)
    y <- as.numeric(y) - 1
    if (length(lvl) == 2) {
      loss <- "binary:logistic"
    } else {
      loss <- "multi:softprob"
    }
  }

  if (is.data.frame(x))
    x <- as.matrix(x) # maybe use model.matrix here?

  n <- nrow(x)
  p <- ncol(x)

  if (!inherits(x, "xgb.DMatrix"))
    x <- xgboost::xgb.DMatrix(x, label = y, missing = NA)
  else
    xgboost::setinfo(x, "label", y)

  # translate `subsample` and `colsample_bytree` to be on (0, 1] if not
  if(subsample > 1)
    subsample <- subsample/n
  if(subsample > 1)
    subsample <- 1

  if(colsample_bytree > 1)
    colsample_bytree <- colsample_bytree/p
  if(colsample_bytree > 1)
    colsample_bytree <- 1

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
    params = arg_list,
    nrounds = nrounds,
    objective = loss
  )
  if (!is.null(num_class))
    main_args$num_class <- num_class

  call <- make_call(fun = "xgb.train", ns = "xgboost", main_args)

  # override or add some other args
  others <- list(...)
  others <-
    others[!(names(others) %in% c("data", "weights", "nrounds", "num_class", names(arg_list)))]
  if (length(others) > 0)
    for (i in names(others))
      call[[i]] <- others[[i]]

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
    "reg:linear" =, "reg:logistic" =, "binary:logistic" = res,
    "binary:logitraw" = stats::binomial()$linkinv(res),
    "multi:softprob" = matrix(res, ncol = object$params$num_class, byrow = TRUE),
    res
  )
  x
}

#' @importFrom purrr map_df
#' @export
multi_predict._xgb.Booster <-
  function(object, new_data, type = NULL, trees = NULL, ...) {
    if (is.null(trees))
      trees <- object$fit$nIter
    trees <- sort(trees)

    if (is.null(type)) {
      if (object$spec$mode == "classification")
        type <- "class"
      else
        type <- "numeric"
    }

    res <-
      map_df(trees, xgb_by_tree, object = object,
             new_data = new_data, type = type, ...)
    res <- arrange(res, .row, trees)
    res <- split(res[, -1], res$.row)
    names(res) <- NULL
    tibble(.pred = res)
  }

xgb_by_tree <- function(tree, object, new_data, type, ...) {
  pred <- xgb_pred(object$fit, newdata = new_data, ntreelimit = tree)

  # switch based on prediction type
  if(object$spec$mode == "regression") {
    pred <- tibble(.pred = pred)
    nms <- names(pred)
  } else {
    if (type == "class") {
      pred <- boost_tree_xgboost_data$classes$post(pred, object)
      pred <- tibble(.pred = factor(pred, levels = object$lvl))
    } else {
      pred <- boost_tree_xgboost_data$prob$post(pred, object)
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

C5.0_train <-
  function(x, y, weights = NULL, trials = 15, minCases = 2, sample = 0, ...) {
    other_args <- list(...)
    protect_ctrl <- c("minCases", "sample")
    protect_fit <- "trials"
    other_args <- other_args[!(other_args %in% c(protect_ctrl, protect_fit))]
    ctrl_args <- other_args[names(other_args) %in% names(formals(C50::C5.0Control))]
    fit_args <- other_args[names(other_args) %in% names(formals(C50::C5.0.default))]

    ctrl <- expr(C50::C5.0Control())
    ctrl$minCases <- minCases
    ctrl$sample <- sample
    for(i in names(ctrl_args))
      ctrl[[i]] <- ctrl_args[[i]]

    fit_call <- expr(C50::C5.0(x = x, y = y))
    fit_call$trials <- trials
    fit_call$control <- ctrl
    if(!is.null(weights))
      fit_call$weights <- quote(weights)

    for(i in names(fit_args))
      fit_call[[i]] <- fit_args[[i]]
    eval_tidy(fit_call)
  }

#' @export
multi_predict._C5.0 <-
  function(object, new_data, type = NULL, trees = NULL, ...) {
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
    pred <- tibble(.pred = factor(pred, levels = object$lvl))
  } else {
    pred <- as_tibble(pred)
    names(pred) <- paste0(".pred_", names(pred))
  }
  nms <- names(pred)
  pred[["trees"]] <- tree
  pred[[".row"]] <- 1:nrow(new_data)
  pred[, c(".row", "trees", nms)]
}
