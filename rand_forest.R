# Prototype parsnip code for random forests

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

###################################################################

rand_forest <- function (x, ...)
  UseMethod("rand_forest")

rand_forest.default <- function(x = NULL, .mode = NULL, ...) {
  
  # parse and save args
  # args <- quos(...)
  # error:
  # Error in captureDots(strict = `__quosured`) :
  #   the argument has already been evaluated
  args <- list(...)
  
  if (is.null(.mode)) {
    .mode <- "unknown"
  }
  # write a constructor function
  out <- list(args = args,
              mode = .mode)
  class(out) <- make_classes("rand_forest", .mode)
  out
}



rand_forest.data.frame <- function(x, y, .mode = NULL, ...) {
  # parse and save args
  args <- quos(...)
  
  # if mode is null, try to set from class(y)
  if (is.null(.mode)) {
    .mode <- guess_mode(y)
  } else {
    # if mode is set, check args and data type
    # check vs available modes
    # check for consistency/appropriateness
  }
  
  # set subclass by mode
  
  # write a constructor function
  out <- list(args = args,
              mode = .mode)
  class(out) <- make_classes("rand_forest", .mode)
  out
}

rand_forest.formula <- function(x, data = stop(), .mode = NULL, ...) {
  mf <- model.frame(x, data = data)
  y <- model.response(mf)
  
  
  # parse and save args
  args <- quos(...)
  
  # if mode is null, try to set from class(y)
  if (is.null(.mode)) {
    .mode <- guess_mode(y)
  } else {
    # if mode is set, check args and data type
    # check vs available modes
    # check for consistency/appropriateness
  }
  
  # set subclass by mode
  
  # write a constructor function
  out <- list(args = args,
              mode = .mode)
  class(out) <- make_classes("rand_forest", .mode)
  out
}

rand_forest.recipe <- function(x, .mode = NULL, ...) {
  inputs <- list(recipe = x)
  var_info <- summary(x)
  y_info <- var_info$type[var_info$role == "outcome"]
  if (length(y_info) > 1)
    stop("Only univariate outcomes are allowed here.", .call = FALSE)
  # parse and save args
  args <- quos(...)
  
  
  if (is.null(.mode)) {
    # make a `guess_mode` method for recipes
    .mode = switch(y_info, numeric = "regression", nominal = "classification")
  } else {
    # if mode is set, check args and data type
    # check vs available modes
    # check for consistency/appropriateness
  }
  
  # set subclass by mode
  # write a constructor function
  out <- list(args = args,
              mode = .mode)
  class(out) <- make_classes("rand_forest", .mode)
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

# Make a conversion table with general arguments (e.g. `num_trees`) and actual for all engines
# for lookups and checks


###################################################################

# Some simple examples

tmp <- rand_forest(x = iris[, 1:4], y = iris$Species, ntree = 200, mtry = param("mtry"))
tmp2 <- rand_forest(mpg ~ ., data = mtcars, ntree = 200, mtry = param("mtry"))
tmp3 <- rand_forest(importance = TRUE)
tmp4 <- rand_forest(recipe(mpg ~ ., data = mtcars), importance = TRUE)


###################################################################

# Before the `fit` function can be executed, create a class that
# will be used to create the specific model code given that a 
# computation engine has been declared. 

fit_code <- function (x, ...)
  UseMethod("fit_code")


fit_code.rand_forest.regression <- function(x, engine = "R::ranger") {
  # check engine
  
  if(engine == "R::ranger") {
    libs <- "ranger"
    interface <- "formula"
    fit_function <- 
      quote(
        ranger(
          formula = NULL,
          data = NULL,
          num.trees = 500,
          mtry = NULL,
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
    # "harmonize" args being passed in
    
    # replace default args with user-specified 
    fit_function <- adjust_expression(
      fit_function,
      x$args
    )
    
  }
  
  # write constructor
  res <- list(library = libs, interface = interface, fit = fit_function)
  class(res) <- "fit_code.rand_forest.regression"
  res
}



