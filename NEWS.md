# parsnip 1.2.1

* Added a missing `tidy()` method for survival analysis glmnet models (#1086).

* A few changes were made to achive more speed-ups (#1075) (#1073) (#1072)

# parsnip 1.2.0

## Bug Fixes

* Tightened logic for outcome checking. This resolves issues—some errors and some silent failures—when atomic outcome variables have an attribute (#1060, #1061).

* Fixed bug in fitting some model types with the `"spark"` engine (#1045).

* Fixed issues in metadata for the `"brulee"` engine where several arguments were mistakenly protected. (#1050, #1054)

* Fixed documentation for `mlp(engine = "brulee")`: the default values for `learn_rate` and `epochs` were swapped (#1018).

* Fixed a bug in the integration with workflows where using a model formula with a formula preprocessor could result in a double intercept (#1033).

## Other Changes

* We no longer add `eval_time` arguments to the prediction specification for the engine (#1039).

* parsnip now lets the engines for [mlp()] check for acceptable values of the activation function (#1019)

* `rpart_train()` has been deprecated in favor of using `decision_tree()` with the `"rpart"` engine or `rpart::rpart()` directly (#1044).

* `.filter_eval_time()` was moved to the survival standalone file. 

* Improved errors and documentation related to special terms in formulas. See `?model_formula` to learn more. (#770, #1014)

* Improved errors in cases where the outcome column is mis-specified. (#1003)

* The `new_data` argument for the `predict()` method for `censoring_model_reverse_km` objects has been deprecated (#965).

* When computing censoring weights, the resulting vectors are no longer named (#1023).

* The `predict()` method for `censoring_model_reverse_km` objects now checks that `...` are empty (#1029).


# parsnip 1.1.1

* Fixed bug where prediction on rank deficient `lm()` models produced `.pred_res` instead of `.pred`. (#985)

* Fixed bug where sparse data was being coerced to non-sparse format doing `predict()`. 

* For BART models with the `dbarts` engine, `predict()` can now also return the standard error for confidence and prediction intervals (#976).

* `augment()` now works for censored regression models. 

* A few censored regression helper functions were exported: `.extract_surv_status()` and `.extract_surv_time()` (#973, #980).

* Fixed bug where `boost_tree()` models couldn't be fit with 1 predictor if `validation` argument was used. (#994)

# parsnip 1.1.0

This release of parsnip contains a number of new features and bug fixes, accompanied by several optimizations that substantially decrease the time to `fit()` and `predict()` with the package.

## Improvements to `"glmnet"` engine interfaces

* glmnet models fitted with base-R family objects are now supported for `linear_reg()`, `logistic_reg()`, and `multinomial_reg()` (#890).

* `multi_predict()` methods for `linear_reg()`, `logistic_reg()`, and `multinom_reg()` models fitted with the `"glmnet"` engine now check the `type` better and error accordingly (#900).

* `.organize_glmnet_pred()` now expects predictions for a single penalty value (#876).

## Survival analysis

* The `time` argument to `predict_survival()` and `predict_hazard()` is deprecated in favor of the new `eval_time` argument (#936).

* Added several internal functions (to help work with `Surv` objects) as a standalone file that can be used in other packages via `usethis::use_standalone("tidymodels/parsnip")`. These changes provide tooling for downstream packages to handle inverse probability censoring weights (#893, #897, #937).

* An internal method for generating inverse probability of censoring weights (IPCW) of Graf _et al_ (1999) is available via `.censoring_weights_graf()`. 

## Bug fixes

* Made `fit()` behave consistently with respect to missingness in the classification setting. Previously, `fit()` erroneously raised an error about the class of the outcome when there were no complete cases, and now always passes along complete cases to be handled by the modeling function (#888).

* Fixed bug where model fits with `engine = "earth"` would fail when the package's namespace hadn't been attached (#251).

* Fixed bug where model fits with factor predictors and `engine = "kknn"` would fail when the package's namespace hadn't been attached (#264).

* Fixed bug with prediction from a boosted tree model fitted with `"xgboost"` using a custom objective function (#875).

## Other changes

* Implemented a number of optimizations in parsnip's backend that [substantially decrease evaluation time](https://www.simonpcouch.com/blog/speedups-2023/#parsnip) to `fit()` and `predict()` (#901, #902, #910, #921, #929, #923, #931, #932, #933).

* `logistic_reg()` will now warn at `fit()` when the outcome has more than two levels (#545).

* Rather than being implemented in each method, the check for the `new_data` argument being mistakenly passed as `newdata` to `multi_predict()` now happens in the generic. Packages re-exporting the `multi_predict()` generic and implementing now-duplicate checks may see new failures and can remove their own analogous checks. This check already existed in all `predict()` methods (via `predict.model_fit()`) and all parsnip `multi_predict()` methods (#525).

* Functions now indicate what class the outcome was if the outcome is the wrong class (#887).

* The minimum version for R is now 3.5 (#926).

* Moved forward with the deprecation of `req_pkgs()` in favor of `required_pkgs()`. The function will now error (#871).

* Transitioned all soft-deprecations that were at least a year old to warn-deprecations. These changes apply to `fit_control()`, `surv_reg()`, `varying()`, `varying_args()`, and the `"liquidSVM"` engine.

* Various bug fixes and improvements to documentation.


# parsnip 1.0.4

* For censored regression models, a "reverse Kaplan-Meier" curve is computed for the censoring distribution. This can be used when evaluating this type of model (#855).

* The model specification methods for `generics::tune_args()` and 
  `generics::tunable()` are now registered unconditionally (tidymodels/workflows#192).

# parsnip 1.0.3

* Adds documentation and tuning infrastructure for the new `flexsurvspline` engine for the `survival_reg()` model specification from the `censored` package (@mattwarkentin, #831).

* The matrix interface for fitting `fit_xy()` now works for the `"censored regression"` mode (#829).

* The `num_leaves` argument of `boost_tree()`s `lightgbm` engine (via the bonsai package) is now tunable.

* A change in our data checking code resulted in about a 3-fold speed-up in parsnip (#835)

# parsnip 1.0.2

* A bagged neural network model was added (`bag_mlp()`). Engine implementations will live in the baguette package. 

* Fixed installation failures due to undocumented knitr installation dependency (#785).

* `fit_xy()` now fails when the model mode is unknown. 

* brulee engine-specific tuning parameters were updated. These changes can be used with dials version > 1.0.0.

* `fit()` and `fit_xy()` doesn't error anymore if `control` argument isn't a `control_parsnip()` object. Will work as long as the object passed to `control` includes the same elements as `control_parsnip()`.

* Improved prompts related to missing (or not loaded) extension packages as well as better handling of model mode conflicts. 


# parsnip 1.0.1

* Enabled passing additional engine arguments with the xgboost `boost_tree()` engine. To supply engine-specific arguments that are documented in `xgboost::xgb.train()` as arguments to be passed via `params`, supply the list elements directly as named arguments to `set_engine()`. Read more in `?details_boost_tree_xgboost` (#787).
  
# parsnip 1.0.0

## Model Specification Changes

* Enable the use of case weights for models that support them. 

* `show_model_info()` now indicates which models can utilize case weights. 

* Model type functions will now message informatively if a needed parsnip extension package is not loaded (#731).

* Refactored internals of model specification printing functions. These changes are non-breaking for extension packages, but the new `print_model_spec()` helper is exported for use in extensions if desired (#739).

## Bug fixes

* Fixed bug where previously set engine arguments would propagate through `update()` methods despite `fresh = TRUE` (#704).

* Fixed a bug where an error would be thrown if arguments to model functions were namespaced (#745).

* `predict(type = "prob")` will now provide an error if the outcome variable has a level called `"class"` (#720).

*  An inconsistency for probability type predictions for two-class GAM models was fixed (#708)

* Fixed translated printing for `null_model()` (#752)

## Other changes

* Added a `glm_grouped()` function to convert long data to the grouped format required by `glm()` for logistic regression. 

* `xgb_train()` now allows for case weights 

* Added `ctree_train()` and `cforest_train()` wrappers for the functions in the partykit package. Engines for these will be added to other parsnip extension packages.  

* Exported `xgb_predict()` which wraps xgboost's `predict()` method for use with parsnip extension packages (#688).

* Added a developer function, `.model_param_name_key` that translates names of tuning parameters.


# parsnip 0.2.1

* Fixed a major bug in spark models induced in the previous version (#671).

* Updated the parsnip add-in with new models and engines. 

* Updated parameter ranges for some `tunable()` methods and added a missing engine argument for brulee models. 

* Added information about how to install the mixOmics package for PLS models (#680)


# parsnip 0.2.0

## Model Specification Changes

* Bayesian additive regression trees (BART) were added via the `bart()` function.

* Added the `"glm"` engine for `linear_reg()` for numeric outcomes (#624).

* Added `brulee` engines for `linear_reg()`, `logistic_reg()`, `multinom_reg()` and `mlp()`. 

## Bug fixes

* A bug for class predictions of two-class GAM models was fixed (#541)

* Fixed a bug for `logistic_reg()` with the LiblineaR engine (#552).

* The list column produced when creating survival probability predictions is now always called `.pred` (with `.pred_survival` being used inside of the list column). 

* Fixed outcome type checking affecting a subset of regression models (#625).

* Prediction using `multinom_reg()` with the `nnet` engine with a single row no longer fails (#612).

## Other Changes

* When the xy interface is used and the underlying model expects to use a matrix, a better warning is issued when predictors contain non-numeric columns (including dates). 
 
* The fit time is only calculated when the `verbosity` argument of `control_parsnip()` is 2L or greater. Also, the call to `system.time()` now uses `gcFirst = FALSE`. (#611)
 
* `fit_control()` is soft-deprecated in favor of `control_parsnip()`. 

* New `extract_parameter_set_dials()` method to extract parameter sets from model specs.

* New `extract_parameter_dials()` method to extract a single parameter from model specs.

* Argument `interval` was added for prediction: For types `"survival"` and `"quantile"`, estimates for the confidence or prediction interval can be added if available (#615).

* `set_dependency()` now allows developers to create package requirements that are specific to the model's mode (#604). 

* `varying()` is soft-deprecated in favor of `tune()`.

* `varying_args()` is soft-deprecated in favor of `tune_args()`.

* An `autoplot()` method was added for glmnet objects, showing the coefficient paths versus the penalty values (#642).

* parsnip is now more robust working with keras and tensorflow for a larger range of versions (#596).

* xgboost engines now use the new `iterationrange` parameter instead of the deprecated `ntreelimit` (#656).  

## Developer

* Models information can be re-registered as long as the information being registered is the same. This is helpful for packages that add new engines and use `devtools::load_all()` (#653).


# parsnip 0.1.7

## Model Specification Changes

* A model function (`gen_additive_mod()`) was added for generalized additive models. 

* Each model now has a default engine that is used when the model is defined. The default for each model is listed in the help documents. This also adds functionality to declare an engine in the model specification function. `set_engine()` is still required if engine-specific arguments need to be added. (#513)

* parsnip now checks for a valid combination of engine and mode (#529)

* The default engine for `multinom_reg()` was changed to `nnet`. 

## Other Changes

* The helper functions `.convert_form_to_xy_fit()`, `.convert_form_to_xy_new()`, `.convert_xy_to_form_fit()`, and  `.convert_xy_to_form_new()` for converting between formula and matrix interface are now exported for developer use (#508).

* Fix bug in `augment()` when non-predictor, non-outcome variables are included in data (#510).

* New article "Fitting and Predicting with parsnip" which contains examples for various combinations of model type and engine. ( #527)

# parsnip 0.1.6

## Model Specification Changes

* A new linear SVM model `svm_linear()` is now available with the `LiblineaR` engine (#424) and the `kernlab` engine (#438), and the `LiblineaR` engine is available for `logistic_reg()` as well (#429). These models can use sparse matrices via `fit_xy()` (#447) and have a `tidy` method (#474).

* For models with `glmnet` engines: 

  - A single value is required for `penalty` (either a single numeric value or a value of `tune()`) (#481).
  - A special argument called `path_values` can be used to set the `lambda` path as a specific set of numbers (independent of the value of `penalty`). A pure ridge regression models (i.e., `mixture = 1`) will generate incorrect values if the path does not include zero. See issue #431 for discussion (#486).
  
* The `liquidSVM` engine for `svm_rbf()` was deprecated due to that package's removal from CRAN. (#425)

* The xgboost engine for boosted trees was translating `mtry` to xgboost's `colsample_bytree`. We now map `mtry` to `colsample_bynode` since that is more consistent with how random forest works. `colsample_bytree` can still be optimized by passing it in as an engine argument. `colsample_bynode` was added to xgboost after the `parsnip` package code was written. (#495)

* For xgboost, `mtry` and `colsample_bytree` can be passed as integer counts or proportions, while `subsample` and `validation` should always be proportions. `xgb_train()` now has a new option `counts` (`TRUE` or `FALSE`) that states which scale for `mtry` and `colsample_bytree` is being used. (#461)  

## Other Changes

* Re-licensed package from GPL-2 to MIT. See [consent from copyright holders here](https://github.com/tidymodels/parsnip/issues/462).

* `set_mode()` now checks if `mode` is compatible with the model class, similar to `new_model_spec()` (@jtlandis, #467). Both `set_mode()` and `set_engine()` now error for `NULL` or missing arguments (#503).

* Re-organized model documentation:

   * `update` methods were moved out of the model help files (#479).
   * Each model/engine combination has its own help page. 
   * The model help page has a dynamic bulleted list of the engines with links to the individual help pages. 

* `generics::required_pkgs()` was extended for `parsnip` objects. 

* Prediction functions now give a consistent error when a user uses an unavailable value of `type` (#489)

* The `augment()` method was changed to avoid failing if the model does not enable class probabilities. The method now returns tibbles despite the input data class (#487) (#478)

* xgboost engines now respect the `event_level` option for predictions (#460).  


# parsnip 0.1.5

* An RStudio add-in is available that makes writing multiple `parsnip` model specifications to the source window. It can be accessed via the IDE addin menus or by calling `parsnip_addin()`.

* For `xgboost` models, users can now pass `objective` to `set_engine("xgboost")`. (#403)

* Changes to test for cases when CRAN cannot get `xgboost` to work on their Solaris configuration.

* There is now an `augument()` method for fitted models. See `augment.model_fit`. (#401)

* Column names for `x` are now required when `fit_xy()` is used. (#398)

* There is now an `event_level` argument for the `xgboost` engine. (#420)

* New mode "censored regression" and new prediction types "linear_pred", "time", "survival", "hazard". (#396)

* Censored regression models cannot use `fit_xy()` (use `fit()`). (#442)

# parsnip 0.1.4

* `show_engines()` will provide information on the current set for a model. 

* For three models (`glmnet`, `xgboost`, and `ranger`), enable sparse matrix use via `fit_xy()` (#373).

* Some added protections were added for function arguments that are dependent on the data dimensions (e.g., `mtry`, `neighbors`, `min_n`, etc). (#184)

* Infrastructure was improved for running `parsnip` models in parallel using PSOCK clusters on Windows. 

# parsnip 0.1.3

 * A `glance()` method for `model_fit` objects was added (#325)

 * Specific `tidy()` methods for `glmnet` models fit via `parsnip` were created so that the coefficients for the specific fitted `parsnip` model are returned.

## Fixes

 * `glmnet` models were fitting two intercepts (#349)
 
 * The various `update()` methods now work with engine-specific parameters. 
 
# parsnip 0.1.2

## Breaking Changes

 * `parsnip` now has options to set specific types of predictor encodings for different models. For example, `ranger` models run using `parsnip` and `workflows` do the same thing by _not_ creating indicator variables. These encodings can be overridden using the `blueprint` options in `workflows`. As a consequence, it is possible to get a different model fit that previous versions of `parsnip`. More details about specific encoding changes are below. (#326)

## Other Changes

 * `tidyr` >= 1.0.0 is now required. 
 
 * SVM models produced by `kernlab` now use the formula method (see breaking change notice above). This change was due to how `ksvm()` made indicator variables for factor predictors (with one-hot encodings). Since the ordinary formula method did not do this, the data are passed as-is to `ksvm()` so that the results are closer to what one would get if `ksmv()` were called directly. 
 
 * MARS models produced by `earth` now use the formula method. 
 
 * For `xgboost`, a one-hot encoding is used when indicator variables are created. 
 
 * Under-the-hood changes were made so that non-standard data arguments in the modeling packages can be accommodated. (#315)
 
## New Features

 * A new main argument was added to `boost_tree()` called `stop_iter` for early stopping. The `xgb_train()` function gained arguments for early stopping and a percentage of data to leave out for a validation set. 
 
 * If `fit()` is used and the underlying model uses a formula, the _actual_ formula is pass to the model (instead of a placeholder). This makes the model call better. 
 
 * A function named `repair_call()` was added. This can help change the underlying models `call` object to better reflect what they would have obtained if the model function had been used directly (instead of via `parsnip`). This is only useful when the user chooses a formula interface and the model uses a formula interface. It will also be of limited use when a recipes is used to construct the feature set in `workflows` or `tune`. 
 
 * The `predict()` function now checks to see if required modeling packages are installed. The packages are loaded (but not attached). (#249) (#308) (tidymodels/workflows#45)
 
 * The function `req_pkgs()` is a user interface to determining the required packages.  (#308)
 
# parsnip 0.1.1

## New Features

 * `liquidSVM` was added as an engine for `svm_rbf()` (#300)

## Fixes

* The error message for missing packages was fixed (#289 and #292)


## Other Changes

* S3 dispatch for `tidy()` was broken on R 4.0. 


# parsnip 0.0.5

## Fixes

* A bug ([#206](https://github.com/tidymodels/parsnip/issues/206) and [#234](https://github.com/tidymodels/parsnip/issues/234)) was fixed that caused an error when predicting with a multinomial `glmnet` model. 

## Other Changes

 * `glmnet` was removed as a dependency since the new version depends on 3.6.0 or greater. Keeping it would constrain `parsnip` to that same requirement. All `glmnet` tests are run locally. 
 
 * A set of internal functions are now exported. These are helpful when creating a new package that registers new model specifications. 
 
## New Features

 * `nnet` was added as an engine to `multinom_reg()` [#209](https://github.com/tidymodels/parsnip/issues/209)

## Breaking Changes

 * There were some mis-mapped parameters (going between `parsnip` and the underlying model function) for `spark` boosted trees and some `keras` models. See [897c927](https://github.com/tidymodels/parsnip/commit/897c92719332caf7344e7c9c8895ac673517d2c8).


# parsnip 0.0.4

## New Features

* The time elapsed during model fitting is stored in the `$elapsed` slot of the parsnip model object, and is printed when the model object is printed.

* Some default parameter ranges were updated for SVM, KNN, and MARS models. 

* The model `udpate()` methods gained a `parameters` argument for cases when the parameters are contained in a tibble or list. 

* `fit_control()` is soft-deprecated in favor of `control_parsnip()`. 

## Fixes

* [A bug](https://github.com/tidymodels/parsnip/issues/222) was fixed standardizing the output column types of `multi_predict` and `predict` for `multinom_reg`.

* [A bug](https://github.com/tidymodels/parsnip/issues/208) was fixed related to using data descriptors and `fit_xy()`. 

* A bug was fixed related to the column names generated by `multi_predict()`. The top-level tibble will always have a column named `.pred` and this list column contains tibbles across sub-models. The column names for these sub-model tibbles will have names consistent with `predict()` (which was previously incorrect). See [43c15db](https://github.com/tidymodels/parsnip/commit/43c15db377ea9ef27483ff209f6bd0e98cb830d2).

* [A bug](https://github.com/tidymodels/parsnip/issues/174) was fixed standardizing the column names of `nnet` class probability predictions.


# parsnip 0.0.3.1

Test case update due to CRAN running extra tests [(#202)](https://github.com/tidymodels/parsnip/issues/202)
 

# parsnip 0.0.3

Unplanned release based on CRAN requirements for Solaris.

## Breaking Changes

 * The method that `parsnip` stores the model information has changed. Any custom models from previous versions will need to use the new method for registering models. The methods are detailed in `?get_model_env` and the [package vignette for adding models](https://parsnip.tidymodels.org/articles/articles/Scratch.html).

 * The mode needs to be declared for models that can be used for more than one mode prior to fitting and/or translation. 

 * For `surv_reg()`, the engine that uses the `survival` package is now called `survival` instead of `survreg`.  

 * For `glmnet` models, the full regularization path is always fit regardless of the value given to `penalty`. Previously, the model was fit with passing `penalty` to `glmnet`'s `lambda` argument and the model could only make predictions at those specific values. [(#195)](https://github.com/tidymodels/parsnip/issues/195)

## New Features

 * `add_rowindex()` can create a column called `.row` to a data frame. 
 
 * If a computational engine is not explicitly set, a default will be used. Each default is documented on the corresponding model page. A warning is issued at fit time unless verbosity is zero.  

 * `nearest_neighbor()` gained a `multi_predict` method. The `multi_predict()` documentation is a little better organized.  
 
 * A suite of internal functions were added to help with upcoming model tuning features.  

 * A `parsnip` object always saved the name(s) of the outcome variable(s) for proper naming of the predicted values. 


# parsnip 0.0.2

Small release driven by changes in `sample()` in the current r-devel. 


## New Features

* A "null model" is now available that fits a predictor-free model (using the mean of the outcome for regression or the mode for classification).  

* `fit_xy()` can take a single column data frame or matrix for `y` without error 

## Other Changes

* `varying_args()` now has a `full` argument to control whether the full set
of possible varying arguments is returned (as opposed to only the arguments
that are actually varying).

* `fit_control()` not returns an S3 method. 

* For classification models, an error occurs if the outcome data are not encoded as factors (#115). 

* The prediction modules (e.g. `predict_class`, `predict_numeric`, etc) were de-exported. These were internal functions that were not to be used by the users and the users were using them. 

 * An event time data set (`check_times`) was included that is the time (in seconds) to run `R CMD check` using the "r-devel-windows-ix86+x86_64` flavor. Packages that errored are censored. 

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
* Data descriptor names have been changed and are now functions. The descriptor definitions for "cols" and "preds" have been switched. 

# parsnip 0.0.0.9003

* `regularization` was changed to `penalty` in a few models to be consistent with [this change](https://tidymodels.github.io/model-implementation-principles/standardized-argument-names.html#tuning-parameters). 
* If a mode is not chosen in the model specification, it is assigned at the time of fit. [51](https://github.com/tidymodels/parsnip/issues/51)
* The underlying modeling packages now are loaded by namespace. There will be some exceptions noted in the documentation for each model. For example, in some `predict` methods, the `earth` package will need to be attached to be fully operational.

# parsnip 0.0.0.9002

* To be consistent with `snake_case`, `newdata` was changed to `new_data`. 
* A `predict_raw` method was added. 

# parsnip 0.0.0.9001

* A package dependency suffered a new change. 

# parsnip 0.0.0.9000

* The `fit` interface was previously used to cover both the x/y interface as well as the formula interface. Now, `fit()` is the formula interface and [`fit_xy()` is for the x/y interface](https://github.com/tidymodels/parsnip/issues/33). 
* Added a `NEWS.md` file to track changes to the package.
* `predict` methods were [overhauled](https://github.com/tidymodels/parsnip/issues/34) to be [consistent](https://github.com/tidymodels/parsnip/issues/41).
* MARS was added. 
