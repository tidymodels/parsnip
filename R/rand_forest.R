# Prototype parsnip code for random forests

# notes: 
# - protect local vars using something like `mtry = model_expr(floor(sqrt(p)))`

#' General Interface for Random Forest Models
#' 
#' `rand_forest` is a way to generate a _specification_ of a model
#'  before fitting and allows the model to be created using
#'  different packages in R or via Spark. The main arguments for the
#'  model are:
#' \itemize{
#'   \item \code{mtry}: The number of predictors that will be
#'   randomly sampled at each split when creating the tree models.
#'   \item \code{trees}: The number of trees contained in the ensemble.
#'   \item \code{min_n}: The minimum number of data points in a node
#'   that are required for the node to be split further.
#' }
#' These arguments are converted to their specific names at the
#'  time that the model is fit. Other options and argument can be
#'  set using the `engine_args` argument. If left to their defaults
#'  here (`NULL`), the values are taken from the underlying model
#'  functions.
#' 
#' The data given to the function are not saved and are only used
#'  to determine the _mode_ of the model. For `rand_forest`, the
#'  possible modes are "regression" and "classification".
#' 
#' The model can be created using the [fit()] function using the
#'  following _engines_:
#' \itemize{
#' \item \pkg{R}:  `"ranger"` or `"randomForests"` 
#' \item \pkg{Spark}: `"spark"`
#' }
#' @param mode A single character string for the type of model.
#'  Possible values for this model are "unknown", "regression", or
#'  "classification".
#' @param engine_args A named list of arguments to be used by the
#'  underlying models (e.g., `ranger::ranger`,
#'  `randomForest::randomForest`, etc.). These are not evaluated
#'  until the model is fit and will be substituted into the model
#'  fit expression.
#' @param mtry An integer for the number of predictors that will
#'  be randomly sampled at each split when creating the tree models.
#' @param trees An integer for the number of trees contained in
#'  the ensemble.
#' @param min_n An integer for the minimum number of data points
#'  in a node that are required for the node to be split further.
#' @param ... Used for method consistency. Any arguments passed to
#'  the ellipses will result in an error. Use `engine_args` instead.
#' @importFrom rlang expr enquo missing_arg
#' @importFrom purrr map_lgl
#' @seealso [varying()], [fit()]
#' @examples 
#' rand_forest(mode = "classification", trees = 2000)
#' 
#' # Parameters can be represented by a placeholder:
#' rand_forest(mode = "regression", mtry = varying())
#' @export

rand_forest <-
  function(mode = "unknown",
           mtry = NULL, trees = NULL, min_n = NULL,
           engine_args = list(), 
           ...) {
    check_empty_ellipse(...)
    if (!(mode %in% rand_forest_modes))
      stop("`mode` should be one of: ",
           paste0("'", rand_forest_modes, "'", collapse = ", "),
           call. = FALSE)
    
    args <- list(
      mtry = rlang::enquo(mtry),
      trees = rlang::enquo(trees),
      min_n = rlang::enquo(min_n)
    )
    
    others <- parse_engine_options(rlang::enquo(engine_args))
    
    # write a constructor function
    out <- list(args = args, others = others, 
                mode = mode, method = NULL, engine = NULL)
    class(out) <- make_classes("rand_forest", mode)
    out
  }

#' @export
print.rand_forest <- function(x, ...) {
  cat("Random Forest Model Specification (", x$mode, ")\n\n", sep = "")
  model_printer(x, ...)
  invisible(x)
}

###################################################################

# Before the `fit` function can be executed, create a class that
# will be used to create the specific model code given that a 
# computation engine has been declared. 

rand_forest_ranger_regression <- function () {
  libs <- "ranger"
  interface <- "formula"
  protect = c("ranger", "formula", "data", "case.weights")
  fit <- 
    quote(
      ranger(
        formula = formula,
        data = data,
        num.trees = 500,
        mtry =  max(floor((ncol(data) - 1) / 3), 1),
        importance = "none",
        write.forest = TRUE,
        probability = FALSE,
        min.node.size = NULL,
        replace = TRUE,
        sample.fraction = ifelse(replace, 1, 0.632),
        case.weights = NULL,
        splitrule = NULL,
        num.random.splits = 1,
        alpha = 0.5,
        minprop = 0.1,
        split.select.weights = NULL,
        always.split.variables = NULL,
        respect.unordered.factors = NULL,
        scale.permutation.importance = FALSE,
        keep.inbag = FALSE,
        holdout = FALSE,
        num.threads = NULL,
        save.memory = FALSE,
        verbose = FALSE,
        seed = sample.int(10^5, 1),
        dependent.variable.name = NULL,
        status.variable.name = NULL,
        classification = NULL
      )
    ) 
  list(library = libs, interface = interface, fit = fit, protect = protect)
}

rand_forest_randomForest_regression <- function () {
  libs <- "randomForest"
  interface <- "data.frame"
  protect = c("randomForest", "x", "y")
  fit <- 
    quote(
      randomForest(
        x = x,
        y = y,
        xtest = NULL,
        ytest = NULL,
        ntree = 500,
        mtry = max(floor(ncol(x) / 3), 1),
        replace = TRUE,
        classwt  = missing_arg(),
        cutoff = missing_arg(),
        strata = missing_arg(),
        sampsize = if (replace) nrow(x) else ceiling(.632 * nrow(x)),
        nodesize = if (!is.null(y) && !is.factor(y)) 5 else 1,
        maxnodes = NULL,
        importance = FALSE,
        localImp = FALSE,
        nPerm = 1,
        proximity = missing_arg(),
        oob.prox = proximity,
        norm.votes = TRUE,
        do.trace = FALSE,
        keep.forest = !is.null(y) && is.null(xtest),
        corr.bias = FALSE,
        keep.inbag = FALSE
      )
    ) 
  list(library = libs, interface = interface, fit = fit, protect = protect)
}

rand_forest_spark_regression <- function () {
  libs <- "sparklyr"
  interface <- "data.frame" # adjust this to something else
  protect = c("x", "formula", "label_col", "features_col")
  fit <- 
    quote(
      ml_random_forest_regressor(
        x = x,
        formula = NULL,
        num_trees = 20L,
        subsampling_rate = 1,
        max_depth = 5L,
        min_instances_per_node = 1L,
        feature_subset_strategy = "auto",
        impurity = "variance",
        min_info_gain = 0,
        max_bins = 32L,
        seed = NULL,
        checkpoint_interval = 10L,
        cache_node_ids = FALSE,
        max_memory_in_mb = 256L,
        features_col = "features",
        label_col = "label",
        prediction_col = "prediction",
        uid = random_string("random_forest_regressor_"),
        ...
      )
    ) 
  list(library = libs, interface = interface, fit = fit, protect = protect)
}

rand_forest_ranger_classification <- function () {
  libs <- "ranger"
  interface <- "formula"
  protect = c("ranger", "formula", "data", "case.weights")  
  fit <- 
    quote(
      ranger(
        formula = formula,
        data = data,
        num.trees = 500,
        mtry = floor(sqrt(ncol(data) - 1)),
        importance = "none",
        write.forest = TRUE,
        probability = TRUE,
        min.node.size = NULL,
        replace = TRUE,
        sample.fraction = ifelse(replace, 1, 0.632),
        case.weights = NULL,
        splitrule = NULL,
        num.random.splits = 1,
        alpha = 0.5,
        minprop = 0.1,
        split.select.weights = NULL,
        always.split.variables = NULL,
        respect.unordered.factors = NULL,
        scale.permutation.importance = FALSE,
        keep.inbag = FALSE,
        holdout = FALSE,
        num.threads = NULL,
        save.memory = FALSE,
        verbose = FALSE,
        seed = sample.int(10^5, 1),
        dependent.variable.name = NULL,
        status.variable.name = NULL,
        classification = NULL
      )
    ) 
  list(library = libs, interface = interface, fit = fit, protect = protect)
}

rand_forest_randomForest_classification <- function () {
  libs <- "randomForest"
  interface <- "data.frame"
  protect = c("randomForest", "x", "y")
  fit <- 
    quote(
      randomForest(
        x = x,
        y = y,
        xtest = NULL,
        ytest = NULL,
        ntree = 500,
        mtry = floor(sqrt(ncol(x))),
        replace = TRUE,
        classwt = missing_arg(),
        cutoff = missing_arg(),
        strata = missing_arg(),
        sampsize = if (replace) nrow(x) else ceiling(.632 * nrow(x)),
        nodesize = if (!is.null(y) && !is.factor(y)) 5 else 1,
        maxnodes = NULL,
        importance = FALSE,
        localImp = FALSE,
        nPerm = 1,
        proximity = missing_arg(),
        oob.prox = proximity,
        norm.votes = TRUE,
        do.trace = FALSE,
        keep.forest = !is.null(y) && is.null(xtest),
        corr.bias = FALSE,
        keep.inbag = FALSE
      )
    ) 
  list(library = libs, interface = interface, fit = fit, protect = protect)
}

rand_forest_spark_regression <- function () {
  libs <- "sparklyr"
  interface <- "data.frame" # adjust this to something else
  protect = c("x", "formula", "label_col", "features_col")
  fit <- 
    quote(
      ml_random_forest_classifier(
        x = x,
        formula = NULL,
        num_trees = 20L,
        subsampling_rate = 1,
        max_depth = 5L,
        min_instances_per_node = 1L,
        feature_subset_strategy = "auto",
        impurity = "gini",
        min_info_gain = 0,
        max_bins = 32L,
        seed = NULL,
        thresholds = NULL,
        checkpoint_interval = 10L,
        cache_node_ids = FALSE,
        max_memory_in_mb = 256L,
        features_col = "features",
        label_col = "label",
        prediction_col = "prediction",
        probability_col = "probability",
        raw_prediction_col = "rawPrediction",
        uid = random_string("random_forest_classifier_"),
        ...
      )
    ) 
  list(library = libs, interface = interface, fit = fit, protect = protect)
}

###################################################################

# finalizing the model consists of:
#
# 1. obtaining the base expression for the model
# 2. converting standardized arguments to their engine-specific names
# 3. substituting in the user-specified argument values
# 4. removing any of the original default arguments
#
# This should be done only when the model is to be fit. 

#' @export
finalize.rand_forest <- function(x, engine = NULL, ...) {
  check_empty_ellipse(...)
  
  x$engine <- engine
  x <- check_engine(x)
  
  x$method <- get_model_objects(x, x$engine)()
  real_args <- deharmonize(x$args, rand_forest_arg_key, x$engine)
  
  if (x$engine == "ranger" &
      any(names(x$others) == "importance") &&
      is.logical(x$others$importance)) {
    warning(
      "ranger's importance value is character (not logical). ",
      "Changing the value to `importance = 'impurity'`.",
      call. = FALSE
    )
    x$others$importance <- "impurity"
  }
  
  # replace default args with user-specified
  x$method$fit <-
    sub_arg_values(x$method$fit, real_args, ignore = x$method$protect)
  
  if (length(x$others) > 0) {
    protected <- names(x$others) %in% x$method$protect
    if (any(protected)) {
      warning(
        "The following options cannot be changed at this time ",
        "and were removed: ",
        paste0("`", names(x$others)[protected], "`", collapse = ", "),
        call. = FALSE
      )
      x$others <- x$others[-which(protected)]
    }
  }
  if (length(x$others) > 0)
    x$method$fit <- sub_arg_values(x$method$fit, x$others, ignore = x$method$protect)
  
  # remove NULL and unmodified argument values
  modifed_args <- names(real_args)[!vapply(real_args, null_value, lgl(1))]
  x$method$fit <- prune_expr(x$method$fit, x$method$protect, c(modifed_args, names(x$others)))
  x
}

###################################################################

#' Update a Random Forest Specification
#' 
#' If parameters need to be modified, this function can be used
#'  in lieu of recreating the object from scratch. 
#'  
#' @export
#' @inheritParams rand_forest
#' @param object A random forest model specification. 
#' @param fresh A logical for whether the arguments should be
#'  modifed in-place of or replaced wholesale. 
#' @return An updated model specification.
#' @examples 
#' model <- rand_forest(mtry = 10, min_n = 3)
#' model
#' 
#' update(model, mtry = 1)
#' 
#' update(model, mtry = 1, fresh = TRUE)
#' @method update rand_forest
#' @export
update.rand_forest <-
  function(object,
           mtry = NULL, trees = NULL, min_n = NULL,
           engine_args = list(),
           fresh = FALSE,
           ...) {
    check_empty_ellipse(...)
    
    args <- list(
      mtry = rlang::enquo(mtry),
      trees = rlang::enquo(trees),
      min_n = rlang::enquo(min_n)
    )
    if (fresh) {
      object$args <- args
    } else {
      null_args <- map_lgl(args, null_value)
      if (any(null_args))
        args <- args[!null_args]
      if (length(args) > 0)
        object$args[names(args)] <- args
    }
    
    others <- parse_engine_options(rlang::enquo(engine_args))
    if (length(others) > 0) {
      if (fresh)
        object$others <- others
      else
        object$others[names(others)] <- others
    }
    
    object
  }


###################################################################

rand_forest_arg_key <- data.frame(
  randomForest = c("mtry", "ntree", "nodesize"),
  ranger = c("mtry", "num.trees", "min.node.size"),
  spark = 
    c("feature_subset_strategy", "num_trees", "min_instances_per_node"),
  stringsAsFactors = FALSE,
  row.names =  c("mtry", "trees", "min_n")
)

rand_forest_modes <- c("classification", "regression", "unknown")

rand_forest_engines <- data.frame(
  ranger =       c(TRUE, TRUE, FALSE),
  randomForest = c(TRUE, TRUE, FALSE),
  spark =        c(TRUE, TRUE, FALSE),
  row.names =  c("classification", "regression", "unknown")
)

