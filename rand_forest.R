# Prototype parsnip code for random forests

# notes: 
# - Have a create_ranger_code function instead of if/thens and generally better organize the code
# - protect local vars using something like `mtry = model_expr(floor(sqrt(p)))`

###################################################################

library(recipes)
library(rlang)

###################################################################

source("functions.R")

###################################################################

rand_forest <- function (x, ...)
  UseMethod("rand_forest")

rand_forest.default <-
  function(x = NULL, mode = "unknown",
           mtry = NULL, trees = NULL, min_n = NULL,
           engine_args = list()) {
    
    args <- list(
      mtry = enquo(mtry),
      trees = enquo(trees),
      min_n = enquo(min_n)
    )
    
    others <- parse_engine_options(enquo(engine_args))
    
    # write a constructor function
    out <- list(args = args, others = others, 
                mode = mode, fit = NULL, engine = NULL,
                state = NULL)
    class(out) <- make_classes("rand_forest", mode)
    out
  }



rand_forest.data.frame <- 
  function(x, y,
           mode = NULL,
           mtry = NULL,  trees = NULL, min_n = NULL,
           engine_args = list()) {
    
    # parse and save args
    args <- list(
      mtry = enquo(mtry),
      trees = enquo(trees),
      min_n = enquo(min_n)
    )
    
    others <- parse_engine_options(enquo(engine_args))
    
    # if mode is null, try to set from class(y)
    if (is.null(mode)) {
      mode <- guess_mode(y)
    } else {
      # if mode is set, check args and data type
      # check vs available modes
      # check for consistency/appropriateness
    }
    
    # set subclass by mode
    
    # write a constructor function
    out <- list(args = args, others = others, 
                mode = mode, method = NULL, engine = NULL,
                state = NULL)
    class(out) <- make_classes("rand_forest", mode)
    out
  }

rand_forest.formula <-
  function(x,
           data = stop(),
           mode = NULL,
           mtry = NULL, trees = NULL, min_n = NULL,
           engine_args = list()) {
    
    mf <- model.frame(x, data = data)
    y <- model.response(mf)
    
    # parse and save args
    args <- list(
      mtry = enquo(mtry),
      trees = enquo(trees),
      min_n = enquo(min_n)
    )
    
    others <- parse_engine_options(enquo(engine_args))
    
    # if mode is null, try to set from class(y)
    if (is.null(mode)) {
      mode <- guess_mode(y)
    } else {
      # if mode is set, check args and data type
      # check vs available modes
      # check for consistency/appropriateness
    }
    
    # set subclass by mode
    
    # write a constructor function
    out <- list(args = args, others = others, 
                mode = mode, method = NULL, engine = NULL,
                state = NULL)
    class(out) <- make_classes("rand_forest", mode)
    out
  }

rand_forest.recipe <-
  function(x,
           mode = NULL,
           mtry = NULL, trees = NULL, min_n = NULL,
           engine_args = list()) {
    
    inputs <- list(recipe = x)
    var_info <- summary(x)
    y_info <- var_info$type[var_info$role == "outcome"]
    if (length(y_info) > 1)
      stop("Only univariate outcomes are allowed here.", .call = FALSE)
    
    # parse and save args
    args <- list(
      mtry = enquo(mtry),
      trees = enquo(trees),
      min_n = enquo(min_n)
    )
    
    others <- parse_engine_options(enquo(engine_args))
    
    if (is.null(mode)) {
      # make a `guess_mode` method for recipes
      mode = switch(y_info, numeric = "regression", nominal = "classification")
    } else {
      # if mode is set, check args and data type
      # check vs available modes
      # check for consistency/appropriateness
    }
    
    # set subclass by mode
    # write a constructor function
    out <- list(args = args, others = others, 
                mode = mode, method = NULL, engine = NULL,
                state = NULL)
    class(out) <- make_classes("rand_forest", mode)
    out
  }

print.rand_forest <- function(x, ...) {
  cat("Random Forest Model Specification (", x$mode, ")\n\n", sep = "")
  non_null_args <- x$args[!vapply(x$args, null_value, lgl(1))]
  if (length(x$args) > 0) {
    cat("Main Arguments:\n")
    non_null_args <- lapply(non_null_args, as.character)
    non_null_args <- lapply(non_null_args, function(x)
      paste0("  ", x[-1], "\n"))
    anms <- names(non_null_args)
    non_null_args <- paste(anms, unlist(non_null_args), sep = ": ")
    cat(non_null_args, sep = "", "\n\n")
  }
  if (length(x$others) > 0) {
    cat("Other Arguments:\n")
    others <- lapply(x$others, function(x) paste(deparse(x), sep = "\n", collapse = "\n"))
    others <- lapply(others, function(x)
      paste0("  ", x, "\n"))
    onms <- names(others)
    others <- paste(onms, unlist(others), sep = ": ")
    cat(others, sep = "", "\n\n")
  }  
  if(!is.null(x$engine)) {
    cat("Computational engine:", x$engine, "\n\n")
  }
  
  invisible(x)
}

###################################################################

# Before the `fit` function can be executed, create a class that
# will be used to create the specific model code given that a 
# computation engine has been declared. 

## Q: If/when extra arguments are added to the call (that would be put
## in the ellipses), when should the ellipses be removed? Maybe right
## before evaluation since `update` might be invoked to change those. 

get_ranger_reg <- function () {
  libs <- "ranger"
  interface <- "formula"
  protect = c("ranger::ranger", "formula", "data", "case.weights")
  fit <- 
  expr(
    ranger::ranger(
      formula = formula,
      data = data,
      num.trees = 500,
      mtry =  max(floor((ncol(dat) - 1) / 3), 1),
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

get_randomForest_reg <- function () {
  libs <- "randomForest"
  interface <- "data.frame"
  protect = c("randomForest::randomForest", "x", "y")
  fit <- 
    expr(
      randomForest::randomForest(
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


get_ranger_class <- function () {
  libs <- "ranger"
  interface <- "formula"
  protect = c("ranger::ranger", "formula", "data", "case.weights")  
  fit <- 
    expr(
      ranger::ranger(
        formula = NULL,
        data = NULL,
        num.trees = 500,
        mtry = floor(sqrt(ncol(dat) - 1)),
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

get_randomForest_class <- function () {
  libs <- "randomForest"
  interface <- "data.frame"
  protect = c("randomForest::randomForest", "x", "y")
  fit <- 
    expr(
      randomForest::randomForest(
        x,
        y = NULL,
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


###################################################################

# finalizing the model consists of:
#
# 1. obtaining the base expression for the model
# 2. converting standardized arguments to their engine-specific names
# 3. substituting in the user-specified argument values
# 4. removing any of the original default arguments
#
# This should be done only when the model is to be fit. 

finalize.rand_forest.regression <- function(x, engine = "R::ranger") {
  # check engine
  
  if(engine == "R::ranger") {
    res <- get_ranger_reg()
    real_args <- deharmonize(x$args, rand_forest_arg_key, "ranger")
    
    if(any(names(x$others) == "importance") && is.logical(x$others$importance)) {
      warning("ranger's importance value is character (not logical). ",
              "Changing the value to `importance = 'impurity'`.",
              call. = FALSE)
      x$others$importance <- "impurity"
    }
    
  } else {
    res <- get_randomForest_reg()
    real_args <- deharmonize(x$args, rand_forest_arg_key, "randomForest")
  }
  
  # replace default args with user-specified 
  x$method$fit <- sub_arg_values(res$fit, real_args)
  
  if(length(x$others) > 0)
    x$method$fit <- sub_arg_values(x$method$fit, x$others)
  
  # remove NULL and unmodified argiment values
  modifed_args <- names(real_args)[!vapply(real_args, null_value, lgl(1))]
  x$method$fit <- prune_expr(x$method$fit, res$protect, c(modifed_args, names(x$others)))
  x
}

finalize.rand_forest.classification <- function(x, engine = "R::ranger") {
  # check engine
  
  if(engine == "R::ranger") {
    res <- get_ranger_class()
    real_args <- deharmonize(x$args, rand_forest_arg_key, "ranger")
    
    if(any(names(x$others) == "importance") && is.logical(x$others$importance)) {
      warning("ranger's importance value is character (not logical). ",
              "Changing the value to `importance = 'impurity'`.",
              call. = FALSE)
      x$others$importance <- "impurity"
      
    }
  } else {
    res <- get_randomForest_class()
    real_args <- deharmonize(x$args, rand_forest_arg_key, "randomForest")
  }
  
  # replace default args with user-specified 
  x$method$fit <- sub_arg_values(res$fit, real_args)
  
  if(length(x$others) > 0)
    x$method$fit <- sub_arg_values(x$method$fit, x$others)
  
  # remove NULL and unmodified argiment values
  modifed_args <- names(real_args)[!vapply(real_args, null_value, lgl(1))]
  x$method$fit <- prune_expr(x$method$fit, res$protect, c(modifed_args, names(x$others)))
  x
}


finalize.rand_forest.unknown <- function(x, engine = "R::ranger") {
  stop("Please specify a mode for the model (e.g. regression, classification) ", 
       "so that the model code can be finalized", call. = FALSE)
}

###################################################################

update.rand_forest <-
  function(object,
           mtry = NULL, trees = NULL, min_n = NULL,
           engine_args = list(),
           fresh = FALSE) {
    
    args <- list(
      mtry = enquo(mtry),
      trees = enquo(trees),
      min_n = enquo(min_n)
    )
    null_args <- map_lgl(args, null_value)
    if (any(null_args))
      args <- args[!null_args]
    if (length(args) > 0) 
      object$args[names(args)] <- args
    
    others <- parse_engine_options(enquo(engine_args))
    if (length(others) > 0) {
      if(fresh)
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
  RandomForestModel = 
    c("feature_subset_strategy", "num_trees", "min_instances_per_node"),
  stringsAsFactors = FALSE
)
rownames(rand_forest_arg_key) <- c("mtry", "trees", "min_n")

###################################################################


# The S3 part here is awful for now

fit_formula <- function(object, formula, data, verboseness = 0, engine = "R::ranger") {
  varying_param_check(object)
  
  # go between input methods =[
  
  # data checks based on method
  
  object <- finalize(object, engine = engine)
  if(verboseness == 0) {
    fit_obj <- eval(object$method$fit)
  } else {
    capture.output(fit_obj <- eval(object$method$fit))
  }
  fit_obj
}

###################################################################

# Some simple examples

tmp <- rand_forest(x = iris[, 1:4], y = iris$Species, trees = 200, mtry = varying())
tmp2 <- rand_forest(mpg ~ ., data = mtcars, trees  = 200, mtry = varying())
tmp3 <- rand_forest(mode = "regression", mtry = 3, min_n = varying(), 
                    engine_args = list(case.weights = floor(nrow(x)/2), num.threads = 3))
tmp4 <- rand_forest(recipe(mpg ~ ., data = mtcars), mtry = 20, min_n = varying(), 
                    engine_args = list(sample.fraction = floor(nrow(x)/2), num.threads = 3))
tmp5 <- rand_forest(recipe(mpg ~ ., data = mtcars), mtry = 3, engine_args = list(importance = TRUE, strata = varying()))

rand_forest(recipe(mpg ~ ., data = mtcars), mtry = 2, min_n = varying(), 
            engine_args = list(num.threads = 3, importance = TRUE, verbose = TRUE)) %>%
  update(min_n = 3) %>%
  fit(mpg ~ ., data = mtcars)

rand_forest(recipe(mpg ~ ., data = mtcars), mtry = 2, min_n = floor(nrow(data)/2), 
            engine_args = list(num.threads = 3)) %>%
  fit(formula = mpg ~ ., data = mtcars)

rf_cars <- rand_forest(recipe(mpg ~ ., data = mtcars), mtry = 2, min_n = 7)

rf_fit_1 <- rf_cars %>%
  fit(formula = mpg ~ ., data = mtcars)
rf_fit_2 <- rf_cars %>%
  fit(formula = mpg ~ ., data = mtcars, engine = "R::randomForest")


