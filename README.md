# parsnip

For modeling in the tidyverse, a common api is needed for model syntax. 


## Existing Approach in `caret`

`caret` does this, but in the most kludgy way passible. It defines lists of code components for a particular model function. For example, for a MARS model via the `earth` package, it defines components for fitting the model, generating predictions, and class probabilities: 

```r
# abbreviated version of `earth` code
modelInfo <-
  list(
    fit = function(x, y, wts, param, lev, last, classProbs, ...) {
      # force `keepxy`
      the_dots <- list(...)
      the_dots$keepxy <- TRUE
      
      ## pass in any model weights
      if (!is.null(wts))
        the_dots$weights <- wts
      
      model_args <- c(
        list(
          x = x,
          y = y,
          degree = param$degree,
          nprune = param$nprune
        ),
        the_dots
      )
      if (is.factor(y))
        model_args$glm <- list(family = binomial)
      
      do.call(earth::earth, model_args)
    },
    predict = function(modelFit, newdata, submodels = NULL) {
      if (modelFit$problemType == "Classification") {
        out <- predict(modelFit, newdata,  type = "class")
      } else {
        out <- predict(modelFit, newdata)
      }
      out
    },
    prob = function(modelFit, newdata, submodels = NULL) {
      out <- predict(modelFit, newdata, type = "response")
      out <- as.data.frame(out)
      out
    }
  )
```

This is a mess for a few reasons, including:

* The biggest issue (to me) is that it limits the arguments that can be parameterized. For example, if we would like to tune the model over some other argument to `earth.default`, a new method would need to be written. 

* Functional arguments (like `nprune = floor(ncol(x) + 1)`) can't be passed in. Everything is evaluated.  

## Goals

* Create an easier to use model library that is more flexible

* Enable model components (e.g. `fit` above) to call other components for that model (like `self.fit` in python would do).

* Standardize on some parameter names/types so that users don't have to remember the minutiae. For example, across random forest packages, the size of the forest might be `n.tree`, `ntrees`, etc. People _hate_ that. 
 
* Starting with a small number of models, decouple the modeling _technique_ from the underlying function. For example, a single decision tree model would be fit using `rpart` or `C50` in R or using `DecisionTreeClassificationModel` in Spark. A general `decision_tree` method would work for each and figure out what compute engine and the software package is used based on other arguments.
 
* Enable deferred evaluation of some function arguments. For example, we want to be able to get the model call(s) but leave some argument values to be evaluated later (over different values). In the MARS example, if we tune over `nprune`, we don't know its exact value when the function expression is created. Being able to say something like `nprune = param("num_terms")` would allow this and enable auto-detection of tuning parameters by other code. 

## Initial Issues

* The model technique workflow across different functions/packages type can be very different. 

* How should code be organized? By compute engine, model type (e.g. classification, regression, survival, etc), R package, etc. It will probably need to be over a few of these categories. 

* For a specific model function, the model workflow could have multiple steps that might include conditional logic. Boosted trees via `xgboost` require a series of statements with parameters that vary based on the data types. The fit component is shown below. Do we create a series of expressions? 

```r
# code for xgboost `fit` component

model_args <- 
  list(
    eta = param$eta,
    max_depth = param$max_depth,
    gamma = param$gamma,
    colsample_bytree = param$colsample_bytree,
    min_child_weight = param$min_child_weight,
    subsample = param$subsample
  )

# Classification models
if (is.factor(y)) {
  if (length(levels(y)) == 2) {
    loss <- "binary:logistic" 
  } else{
    loss <- "multi:softprob"
    # only set this with multiclass!
    model_args$num_class <- length(levels(y))
  }
  
  # xgboost doesn't use factors outcomes =[
  y <- as.numeric(y) - 1
} else {
  loss <- "reg:linear"
}

# requires a specific data type
if (!inherits(x, "xgb.DMatrix"))
  x <- xgboost::xgb.DMatrix(x, label = y, missing = NA)
else
  xgboost::setinfo(x, "label", y)

# Sets weights separately (when needed)
if (!is.null(wts))
  xgboost::setinfo(x, 'weight', wts)

xgb.train(
  params = model_args,
  data = x,
  num_class = length(lev)
  nrounds = param$nrounds,
  objective = "binary:logistic",
  ...
)
```
 
