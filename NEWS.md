# parsnip 0.0.1.9000

## New Features

* A "null model" is now available that fits a predictor-free model (using the mean of the outcome for regression or the mode for classification).  

* `fit_xy()` can take a single column data frame or matrix for `y` without error 

## Other Changes

* `varying_args()` now has a `full` argument to control whether the full set
of possible varying arguments is returned (as opposed to only the arguments
that are actually varying).

* `fit_control()` not returns an S3 method. 

* The prediction modules (e.g. `predict_class`, `predict_numeric`, etc) were de-exported. These were internal functions that were not to be used by the users and the users were using them. 

## Bug Fixes

* `varying_args()` now uses the version from the `generics` package. This means
that the first argument, `x`, has been renamed to `object` to align with 
generics.

* For the recipes step method of `varying_args()`, there is now error checking
to catch if a user tries to specify an argument that _cannot_ be varying as 
varying (for example, the `id`) (#132).

* `find_varying()`, the internal function for detecting varying arguments, 
now returns correct results when a size 0 argument is provided. It can also now
detect varying arguments nested deeply into a call (#131, #134).

* For multinomial regression, the `.pred_` prefix is now only added to prediction
column names once (#107).

* For multinomial regression using glmnet, `multi_predict()` now pulls the 
correct default penalty (#108).

* Confidence and prediction intervals for logistic regression were only computed the intervals for a single level. Both are now computed. (#156)


# parsnip 0.0.1

First CRAN release

# parsnip 0.0.0.9005

* The engine, and any associated arguments, are now specified using `set_engine()`. There is no `engine` argument 


# parsnip 0.0.0.9004

* Arguments to modeling functions are now captured as quosures. 
* `others` has been replaced by `...`
* Data descriptor names have beemn changed and are now functions. The descriptor definitions for "cols" and "preds" have been switched. 

# parsnip 0.0.0.9003

* `regularization` was changed to `penalty` in a few models to be consistent with [this change](tidymodels/model-implementation-principles@08d3afd). 
* If a mode is not chosen in the model specification, it is assigned at the time of fit. [51](https://github.com/topepo/parsnip/issues/51)
* The underlying modeling packages now are loaded by namespace. There will be some exceptions noted in the documentation for each model. For example, in some `predict` methods, the `earth` package will need to be attached to be fully operational.

# parsnip 0.0.0.9002

* To be consistent with `snake_case`, `newdata` was changed to `new_data`. 
* A `predict_raw` method was added. 

# parsnip 0.0.0.9001

* A package dependency suffered a new change. 

# parsnip 0.0.0.9000

* The `fit` interface was previously used to cover both the x/y interface as well as the formula interface. Now, `fit()` is the formula interface and [`fit_xy()` is for the x/y interface](https://github.com/topepo/parsnip/issues/33). 
* Added a `NEWS.md` file to track changes to the package.
* `predict` methods were [overhauled](https://github.com/topepo/parsnip/issues/34) to be [consistent](https://github.com/topepo/parsnip/issues/41).
* MARS was added. 
