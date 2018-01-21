# Prototype parsnip code for random forests

# notes: 
# - Have a create_ranger_code function instead of if/thens and generally better organize the code
# - protect local vars using something like `mtry = model_expr(floor(sqrt(p)))`
# - remove argiments with NULL values from code definition? 


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

parse_engine_options <- function(x) {
  res <- ll()
  if (length(x) >= 2) { # in case of NULL
    
    arg_names <- names(x[[2]])
    arg_names <- arg_names[arg_names != ""]
    
    if (length(arg_names) > 0) {
      # in case of list()
      res <- ll()
      for (i in arg_names) {
        res[[i]] <- x[[2]][[i]]
      } # over arg_names
    } # length == 0
  }
  res
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
    
    others <- parse_engine_options(enquo(engine_args))
    
    # write a constructor function
    out <- list(args = args, others = others, mode = mode, fit = NULL, engine = NULL)
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
    out <- list(args = args, others = others, mode = mode, fit = NULL, engine = NULL)
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
    out <- list(args = args, others = others, mode = mode, fit = NULL, engine = NULL)
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
    out <- list(args = args, others = others, mode = mode, fit = NULL, engine = NULL)
    class(out) <- make_classes("rand_forest", mode)
    out
  }

print.rand_forest <- function(x, ...) {
  cat("Random Forest Model Specification (", x$mode, ")\n\n", sep = "")
  if (length(x$args) > 0) {
    cat("Main Arguments:\n")
    args <- lapply(x$args, as.character)
    args <- lapply(args, function(x)
      paste0("  ", x[-1], "\n"))
    anms <- names(args)
    args <- paste(anms, unlist(args), sep = ": ")
    cat(args, sep = "", "\n\n")
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

## Q: do we need the args and others at this point? Should this 
## be another class?

## Q: add a logical for whether there are any varyings? 

## Q: use "compile" or some other verb here? initialize? 
## formalize? compose? keras::compile is not generic =[

## Q: can we check that the arg value type is valid? For example, 
## in ranger, `importance` is a character string but in randomForest
## it is logical so we might want to check if the value type is correct

## Q: is it okay to add NULL defaults to arguments that have no default? 
## or, how can I pick up those names from the expression? 

## If/when extra arguments are added to the call (that would be put
## in the ellipses), when should the ellipses be removed? Maybe right
## before evaluation since `update` might be invoked to change those. 

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
      verbose = FALSE,
      seed = NULL,
      dependent.variable.name = NULL,
      status.variable.name = NULL,
      classification = NULL
    )
  ) 
  list(library = libs, interface = interface, fit = fit)
}

get_randomForest_reg <- function () {
  libs <- "randomForest"
  interface <- "xy"
  fit <- 
    expr(
      randomForest(
        x,
        y = NULL,
        xtest = NULL,
        ytest = NULL,
        ntree = 500,
        mtry = max(floor(ncol(x) / 3), 1),
        replace = TRUE,
        classwt = NULL,
        cutoff,
        strata,
        sampsize = if (replace) nrow(x) else ceiling(.632 * nrow(x)),
        nodesize = if (!is.null(y) && !is.factor(y)) 5 else 1,
        maxnodes = NULL,
        importance = FALSE,
        localImp = FALSE,
        nPerm = 1,
        proximity,
        oob.prox = proximity,
        norm.votes = TRUE,
        do.trace = FALSE,
        keep.forest = !is.null(y) && is.null(xtest),
        corr.bias = FALSE,
        keep.inbag = FALSE
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
        seed = NULL,
        dependent.variable.name = NULL,
        status.variable.name = NULL,
        classification = NULL
      )
    ) 
  list(library = libs, interface = interface, fit = fit)
}

get_randomForest_class <- function () {
  libs <- "randomForest"
  interface <- "xy"
  fit <- 
    expr(
      randomForest(
        x,
        y = NULL,
        xtest = NULL,
        ytest = NULL,
        ntree = 500,
        mtry = floor(sqrt(ncol(x))),
        replace = TRUE,
        classwt,
        cutoff,
        strata,
        sampsize = if (replace) nrow(x) else ceiling(.632 * nrow(x)),
        nodesize = if (!is.null(y) && !is.factor(y)) 5 else 1,
        maxnodes = NULL,
        importance = FALSE,
        localImp = FALSE,
        nPerm = 1,
        proximity,
        oob.prox = proximity,
        norm.votes = TRUE,
        do.trace = FALSE,
        keep.forest = !is.null(y) && is.null(xtest),
        corr.bias = FALSE,
        keep.inbag = FALSE
      )
    ) 
  list(library = libs, interface = interface, fit = fit)
}


fit_code.rand_forest.regression <- function(x, engine = "R::ranger") {
  # check engine
 
  if(engine == "R::ranger") {
    res <- get_ranger_reg()
    # "harmonize" args being passed in and check for collisions
    args <- deharmonize(x$args, rand_forest_arg_key, "ranger")
  } else {
    res <- get_randomForest_reg()
    args <- deharmonize(x$args, rand_forest_arg_key, "randomForest")
  }
  
  # replace default args with user-specified 
  x$fit <- adjust_expression(res$fit, args)
  
  if(length(x$others) > 0)
    x$fit <- adjust_expression(x$fit, x$others)
  
  x$engine <- engine
  x
}

fit_code.rand_forest.classification <- function(x, engine = "R::ranger") {
  # check engine
  
  if(engine == "R::ranger") {
    res <- get_ranger_class()
    
    # "harmonize" args being passed in and check for collisions
    args <- deharmonize(x$args, rand_forest_arg_key, "ranger")
    
    # replace default args with user-specified 
    x$fit <- adjust_expression(res$fit, args)
    
    if(length(x$others) > 0)
      x$fit <- adjust_expression(x$fit, x$others)
    
  } # end ranger
  
  x$engine <- engine
  x
}


fit_code.rand_forest.unknown <- function(x, engine = "R::ranger") {
  stop("Please specify a mode for the model (e.g. regression, classification) ", 
       "so that the model code can be finalized", call. = FALSE)
}


update.rand_forest <-
  function(object,
           mtry = NULL, trees = NULL, min_n = NULL,
           engine_args = list()) {
    
    if(is.null(object$engine))
      stop("Please specify a computational engine")
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
      null_others <- map_lgl(others, null_value)
      if (any(null_others))
        others <- others[!null_others]
    }
    if (length(others) > 0) 
      object$others[names(others)] <- others
    
    # update non-null values
    if (object$engine == "R::ranger") {
      # "harmonize" args being passed in and check for collisions
      if (length(args) > 0)
        args <- deharmonize(args, rand_forest_arg_key, "ranger")
      
      # replace default args with user-specified
      object$fit <- adjust_expression(object$fit, args)
      
      if (length(others) > 0)
        object$fit <- adjust_expression(object$fit, others)
      
    } # end ranger
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

# Some simple examples

tmp <- rand_forest(x = iris[, 1:4], y = iris$Species, trees = 200, mtry = varying())
tmp2 <- rand_forest(mpg ~ ., data = mtcars, trees  = 200, mtry = varying())
tmp3 <- rand_forest(mode = "regression", mtry = 20, min_n = varying(), 
                    engine_args = list(case.weights = floor(nrow(x)/2), num.threads = 3))
tmp4 <- rand_forest(recipe(mpg ~ ., data = mtcars), mtry = 20, min_n = varying(), 
                    engine_args = list(sample.fraction = floor(nrow(x)/2), num.threads = 3))
tmp5 <- rand_forest(recipe(mpg ~ ., data = mtcars), mtry = 20, engine_args = list(importance = TRUE, strata = varying()))
