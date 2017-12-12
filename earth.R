# Example code for fitting MARS models via the earth package

library(rlang)
library(earth)
source('functions.R')

# First, get the call that we want to work with. For earth, the 
# x/y interface in earth.default is the best choice. 

# Question: How to pull this from the source package and keep 
# it as an expression?

# Note: `pmethod` and `keepxy` are defined differently from the
# package's default values. 
earth_fit <- quote(
  earth(
    x = stop("no 'x' argument"),
    y = stop("no 'y' argument"),
    weights = NULL,
    wp = NULL,
    subset = NULL,
    na.action = na.fail,
    pmethod = "none",
    keepxy = TRUE, 
    trace = 0,
    glm = NULL,
    degree = 1,
    penalty = if (degree > 1) 3 else 2,
    nk = min(200, max(20, 2 * ncol(x))) + 1,
    thresh = 0.001,
    minspan = 0,
    endspan = 0,
    newvar.penalty = 0,
    fast.k = 20,
    fast.beta = 1,
    linpreds = FALSE,
    allowed = NULL,
    nprune = NULL,
    Object = NULL,
    Scale.y = (NCOL(y) == 1),
    Adjust.endspan = 2,
    Force.weights = FALSE,
    Use.beta.cache = TRUE,
    Force.xtx.prune = FALSE,
    Get.leverages = NROW(x) < 1e5,
    Exhaustive.tol = 1e-10,
    ...
  )
)

# Create a wrapper similar to what the user would call where the
# ellipses are for extra arguments to modify in the call. 

earth_wrapper <- function(...) {
  args <- quos(...)
  if(length(list() == 0))
    return(earth_fit)
  adjust_expression(
    expr = earth_fit, 
    args = args,
    removals = c("x", "y", "pmethod", "keepxy")
  )
}

# Example 1: simple modification of arguments with known values
earth_wrapper(degree = 15, nk = 20)

# Example 2: sub in a value of an existing R object
earth_wrapper(glm = list(family = binomial))

# Example 3: an argument not in `earth`'s list (`bogus`) and 
# would be presumably passed to earth's ellipses as well as 
# another argument (`x`) that should _not_ be changed by the user. 
earth_wrapper(bogus = 2, x = iris)

# Example 4: an argument that contains an R object that is part
# of the earth's call arguments
earth_wrapper(nk = (2 * ncol(x) + 1))

# Example 5: pass in a placeholder for a value to be supplied later
earth_wrapper(degree = param("degree"), nk = 20)

# The broader api might be:
# 
#  earth_model(type = "regression", engine = "R", package = "earth")
# 
# where the wrapper figures out what `type` should be based on the 
# data at hand. For example, we could have:


earth_model <- function(x, y, ...) {
  # or go by class(y) using oop
  if (is.factor(y))
    mod <- earth_class_fit(x, y, ...)
  else
    mod <- earth_reg_fit(x, y, ...)
  mod
}

earth_reg_fit <- function(x, y, ...) {
  mod_call <- earth_wrapper(...)
  mod_call <- adjust_expression(
    mod_call, 
    list(x = quote(x), y = quote(y))
  )
  # This fails due to scope (I think).
  # eval_tidy(mod_call)
  mod_call
}

earth_class_fit <- function(x, y, ...) {
  mod_call <- earth_wrapper(...)
  mod_call <- adjust_expression(
    mod_call, 
    list(
      x = quote(x), 
      y = quote(y),
      glm = quote(list(family = binomial))
    )
  )
  mod_call
}

# and the user calls it with

model <- earth_model(x = dat, y = outcome, degree = 1, engine = "R")
# This determines the code for model components (fit, predict, etc)
# any may contain placeholders for some values. It figures out the 
# type of model (regression, classification, etc) unless explictly
# told. 

# If no placeholders:
earth_fit <- fit(model)
# This returns the fitted object (and keeps the model code 
# availible)

test_results <- predict(earth_fit, newdata = dat2)
# uses the `predict` code and the fitted object. 


