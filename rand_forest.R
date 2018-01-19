# Prototype parsnip code for random forests

# notes: 
# Have a create_ranger_code function instead of if/thens
# - protect local vars using something like `mtry = model_expr(floor(sqrt(p)))`

###################################################################

library(recipes)
library(rlang)

###################################################################

source("functions.R")

fit <- function (object, ...) 
  UseMethod("fit")

# make S3 with methods for vector, matrix, and recipe
guess_mode <- function(y) {
  if (inherits(y, c("character", "factor"))) {
    res <- "classification"
  } else if (inherits(y, "numeric")) {
    res <- "regression"
  } else if (inherits(y, "Surv")) {
    res <- "risk regression"
  } else res <- "unknown"
  res
}

make_classes <- function(prefix, mode) {
  cls <- c(paste(prefix, mode, sep = "."), prefix)
  gsub(" ", "_", cls)
}

deharmonize <- function(args, key, engine) {
  nms <- names(args)
  for(i in seq_along(args)) {
    names(args)[i] <- key[ nms[i] , engine ]
  }
  args
}

###################################################################

rand_forest <- function (x, ...)
  UseMethod("rand_forest")

# importance as argument?

rand_forest.default <-
  function(x = NULL, mode = "unknown",
           mtry = NULL, trees = NULL, min_n = NULL,
           engine_args = list()) {
    
    args <- list(
      mtry = enquo(mtry),
      trees = enquo(trees),
      min_n = enquo(min_n)
    )
    
    # write a constructor function
    out <- list(args = args, others = enquo(engine_args), mode = mode)
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
    out <- list(args = args, others = enquo(engine_args), mode = mode)
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
    out <- list(args = args, others = enquo(engine_args), mode = mode)
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
    out <- list(args = args, others = enquo(engine_args), mode = mode)
    class(out) <- make_classes("rand_forest", mode)
    out
  }

print.rand_forest <- function(x, ...) {
  cat("Random Forest Model Specification (", x$mode, ")\n\n", sep = "")
  if (length(x$args) > 0) {
    cat("Arguments:\n")
    args <- lapply(x$args, as.character)
    args <- lapply(args, function(x)
      paste0("  ", x[-1], "\n"))
    anms <- names(args)
    args <- paste(anms, unlist(args), sep = ": ")
    cat(args, sep = "")
  }
  
  invisible(x)
}

###################################################################

# Before the `fit` function can be executed, create a class that
# will be used to create the specific model code given that a 
# computation engine has been declared. 

fit_code <- function (x, ...)
  UseMethod("fit_code")


get_ranger_reg <- function () {
  libs <- "ranger"
  interface <- "formula"
  fit <- 
  expr(
    ranger(
      formula = NULL,
      data = NULL,
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
      verbose = TRUE,
      seed = NULL,
      dependent.variable.name = NULL,
      status.variable.name = NULL,
      classification = NULL
    )
  ) 
  list(library = libs, interface = interface, fit = fit)
}

get_ranger_class <- function () {
  libs <- "ranger"
  interface <- "formula"
  fit <- 
    expr(
      ranger(
        formula = NULL,
        data = NULL,
        num.trees = 500,
        mtry = floor(sqrt(ncol(dat) - 1)),
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
        verbose = TRUE,
        seed = NULL,
        dependent.variable.name = NULL,
        status.variable.name = NULL,
        classification = NULL
      )
    ) 
  list(library = libs, interface = interface, fit = fit)
}

fit_code.rand_forest.regression <- function(x, engine = "R::ranger") {
  # check engine
 
  if(engine == "R::ranger") {
    res <- get_ranger_reg()
    
    # "harmonize" args being passed in and check for collisions
    x$args <- deharmonize(x$args, rand_forest_arg_key, "ranger")
    
    # replace default args with user-specified 
    res$fit <- adjust_expression(res$fit, x$args)
    
  } # end ranger
  
  # write constructor
  class(res) <- "fit_code.rand_forest.regression"
  res
}

fit_code.rand_forest.classification <- function(x, engine = "R::ranger") {
  # check engine
  
  if(engine == "R::ranger") {
    res <- get_ranger_class()
    
    # "harmonize" args being passed in and check for collisions
    x$args <- deharmonize(x$args, rand_forest_arg_key, "ranger")
    
    # replace default args with user-specified 
    res$fit <- adjust_expression(res$fit, x$args)
    
  } # end ranger
  
  # write constructor
  class(res) <- "fit_code.rand_forest.regression"
  res
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

# Some simple examples

tmp <- rand_forest(x = iris[, 1:4], y = iris$Species, trees = 200, mtry = varying())
tmp2 <- rand_forest(mpg ~ ., data = mtcars, trees  = 200, mtry = varying())
tmp3 <- rand_forest(mtry = 20, min_n = varying(), engine_args = list(sampsize = floor(nrow(x)/2)))
tmp4 <- rand_forest(recipe(mpg ~ ., data = mtcars))
