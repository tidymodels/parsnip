# Some Notes on the Design of `parsnip`

`parsnip` is trying to solve the issues of unified interfaces for the myriad R modeling functions that have very heterogeneous interfaces and return values. It defines a set of modules, which are specific tasks, such as 

 * fitting the model
 * obtaining numeric predictions for regression models
 * computing different types of predictions for classification and censored regression models

and so on. The list of modules is likely to grow over time to include variable importance scores and so on,. 

`caret` was written for the same purpose. The approach there was to encapsulate the modules as functions (see [this directory](https://github.com/topepo/caret/tree/master/models/files) for examples). The issue with having these modules as functions are:

 * A lot of code duplication.
 * More difficult to maintain.
 * Any functions in open code had to be a dependency of some sort. This led to a long ago version having about 200 package dependencies which was problematic. 

To get around the last point, `caret` _compiles_ these modules into a large list and saves it in the package as an RData file. This avoids `R CMD check` from noticing that code and triggering warnings about dependencies. 

## Model Fitting Modules

`parsnip` approaches the problem differently and relies more on using `call` objects for the modules. In the simple cases, the fit module is a list that contains information about the module including the package and function name for the call as well as any default options. For example, for logistic regression using `glm`, the module may look like:

```r
list(
  func = c(pkg = "stats", fun = "glm"),
  protect = c("formula", "data", "weights"),
  defaults =
    list(
      family = expr(binomial)
    )
)
```

`func` describes the function call (instead of having it in open code). `protect` identifies the arguments that the user should _not_ be allowed to modify, and `defaults` is a list of values that should be set but the user _can_ override.

To create the model fit call, the `protect` arguments are populated with the appropriate objects (usually from the data set), and `rlang::call2` is used to create a call that can be executed. The `translate` function can be used to show the call prototype if there is need to see it (or debugging). 

In the chunk above, the value of the `family` object is quoted (i.e., `expr(binomial)`). If this is not quotes, R will execute the value of the option when the package is compiled. In this case, the full function definition of the binomial family object will be embedded into the model call. Arguments are frequently quoted when making the call so that data objects or objects that don't exist when the package is compiled will not be embedded. (also see the enviromnets section below)

Additional notes:

 * In cases where the model fit function is not a single function call, a wrapper function can be written to deal with this. See `parsnip::keras_mlp` and `parsnip::xgb_train`. this usually triggers package dependencies though. 
 * The `defaults` argument is not the only place to set defaults. The `translate` method for an model specification gets the last word on arguments. It is also a good place to deal with common argument errors and to make defaults based on the _mode_ of the model (e.g. classification or regression). 
 * Users can also pass in quoted arguments
 
## Environments

One of the first things that the `fit` function does is to make a new environment and store the data set and associated objects. For example:

```r
eval_env <- rlang::env()
eval_env$data <- data
eval_env$formula <- formula
```    

This is designed to avoid any issues when executing the call object on the data using `eval_tidy`. 

Any quoted arguments (such as the `family` example given above) are evaluated in this environment just before the model call is evaluated. For a user passes in an argument that is `floor(nrow(data)/3)`, this will be evaluated at this time in the captured environment. 

Several objects are injected into this environment just before the call is executed. [Data descriptor](https://topepo.github.io/parsnip/reference/descriptors.html) objects, such as the number of columns, are computed and can be used in user-quoted arguments. The previous example can be written as `floor(n_obs/3)`.

Note that formulas bring along copies of the objects from the environment that it was executed in:

```r
> f <- as.formula(Species ~ .)
> attributes(f)
$class
[1] "formula"

$.Environment
<environment: R_GlobalEnv>
```

The same is true for quosures. 

## Prediction Modules

Making predictions is done in a manner similar to fitting models; a call is created in the same way. However, there are additional complexities. 

First, the data or model fit object may require some preprocessing to make the predict function work. This does _not_ include executing a formula method on the data but may include coercing the new data into an appropriate format. It can also be used to check for specific fit object requirements. For example, an additional option is required for the `ranger` package to compute class probabilities. The `pre` element of a prediction module can be used to check that the relavant option is set correctly. 

Second, there is a high likelihood that the results of executing the prediction code will require post-processing to put the results into a usable format. `ranger`, for example, returns an object of specific class that contains the predicted values for the new data. The `post` element of the prediction module would extract this value and put it into a more consistent format. 

The postprocessor can also be used to coerce the results into a [_tidy format_](https://tidymodels.github.io/model-implementation-principles/model-predictions.html#return-values). 

