# Changelog

## parsnip (development version)

- Quantile levels are not dropped when a model specification is modified
  ([\#1304](https://github.com/tidymodels/parsnip/issues/1304))

- xgboost and qrnn engines were added for quantile regression to
  [`boost_tree()`](https://parsnip.tidymodels.org/dev/reference/boost_tree.md)
  and [`mlp()`](https://parsnip.tidymodels.org/dev/reference/mlp.md),
  respectively.
  ([\#1321](https://github.com/tidymodels/parsnip/issues/1321))

- Case weight usage was enabled for the `"nnet"` engines of
  [`mlp()`](https://parsnip.tidymodels.org/dev/reference/mlp.md) and
  [`bag_mlp()`](https://parsnip.tidymodels.org/dev/reference/bag_mlp.md)
  as well as for the `"dbarts"` engine of
  [`bart()`](https://parsnip.tidymodels.org/dev/reference/bart.md).

- All model details files note whether case weights can be used or not.

- We now export the generics for
  [`predict_quantile()`](https://parsnip.tidymodels.org/dev/reference/other_predict.md),
  [`predict_class()`](https://parsnip.tidymodels.org/dev/reference/other_predict.md),
  [`predict_classprob()`](https://parsnip.tidymodels.org/dev/reference/other_predict.md),
  and
  [`predict_hazard()`](https://parsnip.tidymodels.org/dev/reference/other_predict.md).
  ([\#1257](https://github.com/tidymodels/parsnip/issues/1257))

## parsnip 1.4.1

CRAN release: 2026-01-11

- Fixed bug where xgboost models would fail to predict when `trees`
  matched number of trees in model.
  ([\#1316](https://github.com/tidymodels/parsnip/issues/1316))

## parsnip 1.4.0

CRAN release: 2025-12-01

- Fixes issue with running predictions for Decision Trees in Spark
  ([\#1309](https://github.com/tidymodels/parsnip/issues/1309))

- Updates to some boosting tuning parameter information:
  ([\#1306](https://github.com/tidymodels/parsnip/issues/1306))

  - lightgbm and catboost have smaller default ranges for the learning
    rate: -3 to -1 / 2 in log10 units.
  - lightgbm, xgboost, catboost, and C5.0 have smaller default ranges
    for the sampling proportion: 0.5 to 1.0.
  - catboost engine arguments were added for `max_leaves` and
    `l2_leaf_reg`.

- Enable generalized random forest (`grf`) models for classification,
  regression, and quantile regression modes.
  ([\#1288](https://github.com/tidymodels/parsnip/issues/1288))

- [`surv_reg()`](https://parsnip.tidymodels.org/dev/reference/surv_reg.md)
  is now defunct and will error if called. Please use
  [`survival_reg()`](https://parsnip.tidymodels.org/dev/reference/survival_reg.md)
  instead ([\#1206](https://github.com/tidymodels/parsnip/issues/1206)).

- Enable parsnip to work with xgboost version \> 2.0.0.0.
  ([\#1227](https://github.com/tidymodels/parsnip/issues/1227))

## parsnip 1.3.3

CRAN release: 2025-08-31

- Bug fix in how tunable parameters were configured for brulee neural
  networks.

- A change to make linear SVM models more quiet.

- A few default parameter ranges were changed for brulee neural network
  models.

## parsnip 1.3.2

CRAN release: 2025-05-28

- Switch to base R pipe

- Requires changes for CRAN’s “No Suggests” check.

- Avoid issues with reading from package files.
  ([\#1271](https://github.com/tidymodels/parsnip/issues/1271))

## parsnip 1.3.1

CRAN release: 2025-03-11

### Bug Fixes

- Fixed a bug that errored when tidying a glmnet object with a penalty
  value greater than one
  ([\#1261](https://github.com/tidymodels/parsnip/issues/1261)).

## parsnip 1.3.0

CRAN release: 2025-02-14

### New Features

- A new model mode (`"quantile regression"`) was added. Including:

  - A
    [`linear_reg()`](https://parsnip.tidymodels.org/dev/reference/linear_reg.md)
    engine for `"quantreg"`.
  - Predictions are encoded via a custom vector type. See
    \[hardhat::quantile_pred()\].
  - Predicted quantile levels are designated when the new mode is
    specified. See
    [`?set_mode`](https://parsnip.tidymodels.org/dev/reference/set_args.md).

- Updates for sparse data formats:

  - [`fit_xy()`](https://generics.r-lib.org/reference/fit_xy.html) can
    now take dgCMatrix input for `x` argument
    ([\#1121](https://github.com/tidymodels/parsnip/issues/1121)).
  - [`fit_xy()`](https://generics.r-lib.org/reference/fit_xy.html) can
    now take sparse tibbles as data values
    ([\#1165](https://github.com/tidymodels/parsnip/issues/1165)).
  - [`predict()`](https://rdrr.io/r/stats/predict.html) can now take
    dgCMatrix and sparse tibble input for `new_data` argument, and error
    informatively when model doesn’t support it
    ([\#1167](https://github.com/tidymodels/parsnip/issues/1167)).

- New
  [`extract_fit_time()`](https://hardhat.tidymodels.org/reference/hardhat-extract.html)
  method has been added that returns the time it took to train the model
  ([\#853](https://github.com/tidymodels/parsnip/issues/853)).

- [`mlp()`](https://parsnip.tidymodels.org/dev/reference/mlp.md) with
  `keras` engine now work for all activation functions currently
  supported by `keras`
  ([\#1127](https://github.com/tidymodels/parsnip/issues/1127)).

- [`mlp()`](https://parsnip.tidymodels.org/dev/reference/mlp.md) now has
  a `brulee_two_layer` engine.

### Other Changes

- Transitioned package errors and warnings to use cli
  ([\#1147](https://github.com/tidymodels/parsnip/issues/1147) and
  [\#1148](https://github.com/tidymodels/parsnip/issues/1148) by
  [@shum461](https://github.com/shum461),
  [\#1153](https://github.com/tidymodels/parsnip/issues/1153) by
  [@RobLBaker](https://github.com/RobLBaker) and
  [@wright13](https://github.com/wright13),
  [\#1154](https://github.com/tidymodels/parsnip/issues/1154) by
  [@JamesHWade](https://github.com/JamesHWade),
  [\#1160](https://github.com/tidymodels/parsnip/issues/1160),
  [\#1161](https://github.com/tidymodels/parsnip/issues/1161),
  [\#1081](https://github.com/tidymodels/parsnip/issues/1081)).

- [`fit_xy()`](https://generics.r-lib.org/reference/fit_xy.html)
  currently raises an error for
  [`gen_additive_mod()`](https://parsnip.tidymodels.org/dev/reference/gen_additive_mod.md)
  model specifications as the default engine (`"mgcv"`) specifies
  smoothing terms in model formulas. However, some engines specify
  smooths via additional arguments, in which case the restriction on
  [`fit_xy()`](https://generics.r-lib.org/reference/fit_xy.html) is
  excessive. parsnip will now only raise an error when fitting a
  [`gen_additive_mod()`](https://parsnip.tidymodels.org/dev/reference/gen_additive_mod.md)
  with [`fit_xy()`](https://generics.r-lib.org/reference/fit_xy.html)
  when using the `"mgcv"` engine
  ([\#775](https://github.com/tidymodels/parsnip/issues/775)).

- Aligned
  [`null_model()`](https://parsnip.tidymodels.org/dev/reference/null_model.md)
  with other model types; the model type now has an engine argument that
  defaults to `"parsnip"` and is checked with the same machinery that
  checks other model types in the package
  ([\#1083](https://github.com/tidymodels/parsnip/issues/1083)).

- If linear regression is requested with a Poisson family, an error will
  occur and refer the user to
  [`poisson_reg()`](https://parsnip.tidymodels.org/dev/reference/poisson_reg.md)
  ([\#1219](https://github.com/tidymodels/parsnip/issues/1219)).

- The deprecated function `rpart_train()` was removed after its
  deprecation period
  ([\#1044](https://github.com/tidymodels/parsnip/issues/1044)).

### Bug Fixes

- Make sure that parsnip does not convert ordered factor predictions to
  be unordered.

- Ensure that
  [`knit_engine_docs()`](https://parsnip.tidymodels.org/dev/reference/knit_engine_docs.md)
  has the required packages installed
  ([\#1156](https://github.com/tidymodels/parsnip/issues/1156)).

- Fixed bug where some models fit using
  [`fit_xy()`](https://generics.r-lib.org/reference/fit_xy.html)
  couldn’t predict
  ([\#1166](https://github.com/tidymodels/parsnip/issues/1166)).

- Fixed bug related to using local (non-package) models
  ([\#1229](https://github.com/tidymodels/parsnip/issues/1229))

- [`tunable()`](https://generics.r-lib.org/reference/tunable.html) now
  references a dials object for the `mixture` parameter
  ([\#1236](https://github.com/tidymodels/parsnip/issues/1236))

### Breaking Change

- For quantile prediction, the `quantile` argument to
  [`predict()`](https://rdrr.io/r/stats/predict.html) has been deprecate
  in facor of `quantile_levels`. This does not affect models with mode
  `"quantile regression"`.

- The quantile regression prediction type was disabled for the
  deprecated
  [`surv_reg()`](https://parsnip.tidymodels.org/dev/reference/surv_reg.md)
  model.

- `NULL` is no longer accepted as an engine
  ([\#1242](https://github.com/tidymodels/parsnip/issues/1242)).

## parsnip 1.2.1

CRAN release: 2024-03-22

- Added a missing
  [`tidy()`](https://generics.r-lib.org/reference/tidy.html) method for
  survival analysis glmnet models
  ([\#1086](https://github.com/tidymodels/parsnip/issues/1086)).

- A few changes were made to achive more speed-ups
  ([\#1075](https://github.com/tidymodels/parsnip/issues/1075))
  ([\#1073](https://github.com/tidymodels/parsnip/issues/1073))
  ([\#1072](https://github.com/tidymodels/parsnip/issues/1072))

## parsnip 1.2.0

CRAN release: 2024-02-16

### Bug Fixes

- Tightened logic for outcome checking. This resolves issues—some errors
  and some silent failures—when atomic outcome variables have an
  attribute
  ([\#1060](https://github.com/tidymodels/parsnip/issues/1060),
  [\#1061](https://github.com/tidymodels/parsnip/issues/1061)).

- Fixed bug in fitting some model types with the `"spark"` engine
  ([\#1045](https://github.com/tidymodels/parsnip/issues/1045)).

- Fixed issues in metadata for the `"brulee"` engine where several
  arguments were mistakenly protected.
  ([\#1050](https://github.com/tidymodels/parsnip/issues/1050),
  [\#1054](https://github.com/tidymodels/parsnip/issues/1054))

- Fixed documentation for `mlp(engine = "brulee")`: the default values
  for `learn_rate` and `epochs` were swapped
  ([\#1018](https://github.com/tidymodels/parsnip/issues/1018)).

- Fixed a bug in the integration with workflows where using a model
  formula with a formula preprocessor could result in a double intercept
  ([\#1033](https://github.com/tidymodels/parsnip/issues/1033)).

### Other Changes

- We no longer add `eval_time` arguments to the prediction specification
  for the engine
  ([\#1039](https://github.com/tidymodels/parsnip/issues/1039)).

- parsnip now lets the engines for \[mlp()\] check for acceptable values
  of the activation function
  ([\#1019](https://github.com/tidymodels/parsnip/issues/1019))

- `rpart_train()` has been deprecated in favor of using
  [`decision_tree()`](https://parsnip.tidymodels.org/dev/reference/decision_tree.md)
  with the `"rpart"` engine or
  [`rpart::rpart()`](https://rdrr.io/pkg/rpart/man/rpart.html) directly
  ([\#1044](https://github.com/tidymodels/parsnip/issues/1044)).

- `.filter_eval_time()` was moved to the survival standalone file.

- Improved errors and documentation related to special terms in
  formulas. See
  [`?model_formula`](https://parsnip.tidymodels.org/dev/reference/model_formula.md)
  to learn more.
  ([\#770](https://github.com/tidymodels/parsnip/issues/770),
  [\#1014](https://github.com/tidymodels/parsnip/issues/1014))

- Improved errors in cases where the outcome column is mis-specified.
  ([\#1003](https://github.com/tidymodels/parsnip/issues/1003))

- The `new_data` argument for the
  [`predict()`](https://rdrr.io/r/stats/predict.html) method for
  `censoring_model_reverse_km` objects has been deprecated
  ([\#965](https://github.com/tidymodels/parsnip/issues/965)).

- When computing censoring weights, the resulting vectors are no longer
  named ([\#1023](https://github.com/tidymodels/parsnip/issues/1023)).

- The [`predict()`](https://rdrr.io/r/stats/predict.html) method for
  `censoring_model_reverse_km` objects now checks that `...` are empty
  ([\#1029](https://github.com/tidymodels/parsnip/issues/1029)).

## parsnip 1.1.1

CRAN release: 2023-08-17

- Fixed bug where prediction on rank deficient
  [`lm()`](https://rdrr.io/r/stats/lm.html) models produced `.pred_res`
  instead of `.pred`.
  ([\#985](https://github.com/tidymodels/parsnip/issues/985))

- Fixed bug where sparse data was being coerced to non-sparse format
  doing [`predict()`](https://rdrr.io/r/stats/predict.html).

- For BART models with the `dbarts` engine,
  [`predict()`](https://rdrr.io/r/stats/predict.html) can now also
  return the standard error for confidence and prediction intervals
  ([\#976](https://github.com/tidymodels/parsnip/issues/976)).

- [`augment()`](https://generics.r-lib.org/reference/augment.html) now
  works for censored regression models.

- A few censored regression helper functions were exported:
  [`.extract_surv_status()`](https://parsnip.tidymodels.org/dev/reference/dot-extract_surv_status.md)
  and
  [`.extract_surv_time()`](https://parsnip.tidymodels.org/dev/reference/dot-extract_surv_time.md)
  ([\#973](https://github.com/tidymodels/parsnip/issues/973),
  [\#980](https://github.com/tidymodels/parsnip/issues/980)).

- Fixed bug where
  [`boost_tree()`](https://parsnip.tidymodels.org/dev/reference/boost_tree.md)
  models couldn’t be fit with 1 predictor if `validation` argument was
  used. ([\#994](https://github.com/tidymodels/parsnip/issues/994))

## parsnip 1.1.0

CRAN release: 2023-04-12

This release of parsnip contains a number of new features and bug fixes,
accompanied by several optimizations that substantially decrease the
time to [`fit()`](https://generics.r-lib.org/reference/fit.html) and
[`predict()`](https://rdrr.io/r/stats/predict.html) with the package.

### Improvements to `"glmnet"` engine interfaces

- glmnet models fitted with base-R family objects are now supported for
  [`linear_reg()`](https://parsnip.tidymodels.org/dev/reference/linear_reg.md),
  [`logistic_reg()`](https://parsnip.tidymodels.org/dev/reference/logistic_reg.md),
  and `multinomial_reg()`
  ([\#890](https://github.com/tidymodels/parsnip/issues/890)).

- [`multi_predict()`](https://parsnip.tidymodels.org/dev/reference/multi_predict.md)
  methods for
  [`linear_reg()`](https://parsnip.tidymodels.org/dev/reference/linear_reg.md),
  [`logistic_reg()`](https://parsnip.tidymodels.org/dev/reference/logistic_reg.md),
  and
  [`multinom_reg()`](https://parsnip.tidymodels.org/dev/reference/multinom_reg.md)
  models fitted with the `"glmnet"` engine now check the `type` better
  and error accordingly
  ([\#900](https://github.com/tidymodels/parsnip/issues/900)).

- [`.organize_glmnet_pred()`](https://parsnip.tidymodels.org/dev/reference/glmnet_helpers_prediction.md)
  now expects predictions for a single penalty value
  ([\#876](https://github.com/tidymodels/parsnip/issues/876)).

### Survival analysis

- The `time` argument to
  [`predict_survival()`](https://parsnip.tidymodels.org/dev/reference/other_predict.md)
  and
  [`predict_hazard()`](https://parsnip.tidymodels.org/dev/reference/other_predict.md)
  is deprecated in favor of the new `eval_time` argument
  ([\#936](https://github.com/tidymodels/parsnip/issues/936)).

- Added several internal functions (to help work with `Surv` objects) as
  a standalone file that can be used in other packages via
  `usethis::use_standalone("tidymodels/parsnip")`. These changes provide
  tooling for downstream packages to handle inverse probability
  censoring weights
  ([\#893](https://github.com/tidymodels/parsnip/issues/893),
  [\#897](https://github.com/tidymodels/parsnip/issues/897),
  [\#937](https://github.com/tidymodels/parsnip/issues/937)).

- An internal method for generating inverse probability of censoring
  weights (IPCW) of Graf *et al* (1999) is available via
  [`.censoring_weights_graf()`](https://parsnip.tidymodels.org/dev/reference/censoring_weights.md).

### Bug fixes

- Made [`fit()`](https://generics.r-lib.org/reference/fit.html) behave
  consistently with respect to missingness in the classification
  setting. Previously,
  [`fit()`](https://generics.r-lib.org/reference/fit.html) erroneously
  raised an error about the class of the outcome when there were no
  complete cases, and now always passes along complete cases to be
  handled by the modeling function
  ([\#888](https://github.com/tidymodels/parsnip/issues/888)).

- Fixed bug where model fits with `engine = "earth"` would fail when the
  package’s namespace hadn’t been attached
  ([\#251](https://github.com/tidymodels/parsnip/issues/251)).

- Fixed bug where model fits with factor predictors and
  `engine = "kknn"` would fail when the package’s namespace hadn’t been
  attached ([\#264](https://github.com/tidymodels/parsnip/issues/264)).

- Fixed bug with prediction from a boosted tree model fitted with
  `"xgboost"` using a custom objective function
  ([\#875](https://github.com/tidymodels/parsnip/issues/875)).

### Other changes

- Implemented a number of optimizations in parsnip’s backend that
  [substantially decrease evaluation
  time](https://www.simonpcouch.com/blog/speedups-2023/#parsnip) to
  [`fit()`](https://generics.r-lib.org/reference/fit.html) and
  [`predict()`](https://rdrr.io/r/stats/predict.html)
  ([\#901](https://github.com/tidymodels/parsnip/issues/901),
  [\#902](https://github.com/tidymodels/parsnip/issues/902),
  [\#910](https://github.com/tidymodels/parsnip/issues/910),
  [\#921](https://github.com/tidymodels/parsnip/issues/921),
  [\#929](https://github.com/tidymodels/parsnip/issues/929),
  [\#923](https://github.com/tidymodels/parsnip/issues/923),
  [\#931](https://github.com/tidymodels/parsnip/issues/931),
  [\#932](https://github.com/tidymodels/parsnip/issues/932),
  [\#933](https://github.com/tidymodels/parsnip/issues/933)).

- [`logistic_reg()`](https://parsnip.tidymodels.org/dev/reference/logistic_reg.md)
  will now warn at
  [`fit()`](https://generics.r-lib.org/reference/fit.html) when the
  outcome has more than two levels
  ([\#545](https://github.com/tidymodels/parsnip/issues/545)).

- Rather than being implemented in each method, the check for the
  `new_data` argument being mistakenly passed as `newdata` to
  [`multi_predict()`](https://parsnip.tidymodels.org/dev/reference/multi_predict.md)
  now happens in the generic. Packages re-exporting the
  [`multi_predict()`](https://parsnip.tidymodels.org/dev/reference/multi_predict.md)
  generic and implementing now-duplicate checks may see new failures and
  can remove their own analogous checks. This check already existed in
  all [`predict()`](https://rdrr.io/r/stats/predict.html) methods (via
  [`predict.model_fit()`](https://parsnip.tidymodels.org/dev/reference/predict.model_fit.md))
  and all parsnip
  [`multi_predict()`](https://parsnip.tidymodels.org/dev/reference/multi_predict.md)
  methods ([\#525](https://github.com/tidymodels/parsnip/issues/525)).

- Functions now indicate what class the outcome was if the outcome is
  the wrong class
  ([\#887](https://github.com/tidymodels/parsnip/issues/887)).

- The minimum version for R is now 3.5
  ([\#926](https://github.com/tidymodels/parsnip/issues/926)).

- Moved forward with the deprecation of
  [`req_pkgs()`](https://parsnip.tidymodels.org/dev/reference/req_pkgs.md)
  in favor of
  [`required_pkgs()`](https://generics.r-lib.org/reference/required_pkgs.html).
  The function will now error
  ([\#871](https://github.com/tidymodels/parsnip/issues/871)).

- Transitioned all soft-deprecations that were at least a year old to
  warn-deprecations. These changes apply to
  [`fit_control()`](https://parsnip.tidymodels.org/dev/reference/fit_control.md),
  [`surv_reg()`](https://parsnip.tidymodels.org/dev/reference/surv_reg.md),
  [`varying()`](https://parsnip.tidymodels.org/dev/reference/varying.md),
  [`varying_args()`](https://generics.r-lib.org/reference/varying_args.html),
  and the `"liquidSVM"` engine.

- Various bug fixes and improvements to documentation.

## parsnip 1.0.4

CRAN release: 2023-02-22

- For censored regression models, a “reverse Kaplan-Meier” curve is
  computed for the censoring distribution. This can be used when
  evaluating this type of model
  ([\#855](https://github.com/tidymodels/parsnip/issues/855)).

- The model specification methods for
  [`generics::tune_args()`](https://generics.r-lib.org/reference/tune_args.html)
  and
  [`generics::tunable()`](https://generics.r-lib.org/reference/tunable.html)
  are now registered unconditionally (tidymodels/workflows#192).

## parsnip 1.0.3

CRAN release: 2022-11-11

- Adds documentation and tuning infrastructure for the new
  `flexsurvspline` engine for the
  [`survival_reg()`](https://parsnip.tidymodels.org/dev/reference/survival_reg.md)
  model specification from the `censored` package
  ([@mattwarkentin](https://github.com/mattwarkentin),
  [\#831](https://github.com/tidymodels/parsnip/issues/831)).

- The matrix interface for fitting
  [`fit_xy()`](https://generics.r-lib.org/reference/fit_xy.html) now
  works for the `"censored regression"` mode
  ([\#829](https://github.com/tidymodels/parsnip/issues/829)).

- The `num_leaves` argument of
  [`boost_tree()`](https://parsnip.tidymodels.org/dev/reference/boost_tree.md)s
  `lightgbm` engine (via the bonsai package) is now tunable.

- A change in our data checking code resulted in about a 3-fold speed-up
  in parsnip ([\#835](https://github.com/tidymodels/parsnip/issues/835))

## parsnip 1.0.2

CRAN release: 2022-10-01

- A bagged neural network model was added
  ([`bag_mlp()`](https://parsnip.tidymodels.org/dev/reference/bag_mlp.md)).
  Engine implementations will live in the baguette package.

- Fixed installation failures due to undocumented knitr installation
  dependency
  ([\#785](https://github.com/tidymodels/parsnip/issues/785)).

- [`fit_xy()`](https://generics.r-lib.org/reference/fit_xy.html) now
  fails when the model mode is unknown.

- brulee engine-specific tuning parameters were updated. These changes
  can be used with dials version \> 1.0.0.

- [`fit()`](https://generics.r-lib.org/reference/fit.html) and
  [`fit_xy()`](https://generics.r-lib.org/reference/fit_xy.html) doesn’t
  error anymore if `control` argument isn’t a
  [`control_parsnip()`](https://parsnip.tidymodels.org/dev/reference/control_parsnip.md)
  object. Will work as long as the object passed to `control` includes
  the same elements as
  [`control_parsnip()`](https://parsnip.tidymodels.org/dev/reference/control_parsnip.md).

- Improved prompts related to missing (or not loaded) extension packages
  as well as better handling of model mode conflicts.

## parsnip 1.0.1

CRAN release: 2022-08-18

- Enabled passing additional engine arguments with the xgboost
  [`boost_tree()`](https://parsnip.tidymodels.org/dev/reference/boost_tree.md)
  engine. To supply engine-specific arguments that are documented in
  [`xgboost::xgb.train()`](https://rdrr.io/pkg/xgboost/man/xgb.train.html)
  as arguments to be passed via `params`, supply the list elements
  directly as named arguments to
  [`set_engine()`](https://parsnip.tidymodels.org/dev/reference/set_engine.md).
  Read more in
  [`?details_boost_tree_xgboost`](https://parsnip.tidymodels.org/dev/reference/details_boost_tree_xgboost.md)
  ([\#787](https://github.com/tidymodels/parsnip/issues/787)).

## parsnip 1.0.0

CRAN release: 2022-06-16

### Model Specification Changes

- Enable the use of case weights for models that support them.

- [`show_model_info()`](https://parsnip.tidymodels.org/dev/reference/set_new_model.md)
  now indicates which models can utilize case weights.

- Model type functions will now message informatively if a needed
  parsnip extension package is not loaded
  ([\#731](https://github.com/tidymodels/parsnip/issues/731)).

- Refactored internals of model specification printing functions. These
  changes are non-breaking for extension packages, but the new
  [`print_model_spec()`](https://parsnip.tidymodels.org/dev/reference/add_on_exports.md)
  helper is exported for use in extensions if desired
  ([\#739](https://github.com/tidymodels/parsnip/issues/739)).

### Bug fixes

- Fixed bug where previously set engine arguments would propagate
  through [`update()`](https://rdrr.io/r/stats/update.html) methods
  despite `fresh = TRUE`
  ([\#704](https://github.com/tidymodels/parsnip/issues/704)).

- Fixed a bug where an error would be thrown if arguments to model
  functions were namespaced
  ([\#745](https://github.com/tidymodels/parsnip/issues/745)).

- `predict(type = "prob")` will now provide an error if the outcome
  variable has a level called `"class"`
  ([\#720](https://github.com/tidymodels/parsnip/issues/720)).

- An inconsistency for probability type predictions for two-class GAM
  models was fixed
  ([\#708](https://github.com/tidymodels/parsnip/issues/708))

- Fixed translated printing for
  [`null_model()`](https://parsnip.tidymodels.org/dev/reference/null_model.md)
  ([\#752](https://github.com/tidymodels/parsnip/issues/752))

### Other changes

- Added a
  [`glm_grouped()`](https://parsnip.tidymodels.org/dev/reference/glm_grouped.md)
  function to convert long data to the grouped format required by
  [`glm()`](https://rdrr.io/r/stats/glm.html) for logistic regression.

- [`xgb_train()`](https://parsnip.tidymodels.org/dev/reference/xgb_train.md)
  now allows for case weights

- Added
  [`ctree_train()`](https://parsnip.tidymodels.org/dev/reference/ctree_train.md)
  and
  [`cforest_train()`](https://parsnip.tidymodels.org/dev/reference/ctree_train.md)
  wrappers for the functions in the partykit package. Engines for these
  will be added to other parsnip extension packages.

- Exported
  [`xgb_predict()`](https://parsnip.tidymodels.org/dev/reference/xgb_train.md)
  which wraps xgboost’s
  [`predict()`](https://rdrr.io/r/stats/predict.html) method for use
  with parsnip extension packages
  ([\#688](https://github.com/tidymodels/parsnip/issues/688)).

- Added a developer function, `.model_param_name_key` that translates
  names of tuning parameters.

## parsnip 0.2.1

CRAN release: 2022-03-17

- Fixed a major bug in spark models induced in the previous version
  ([\#671](https://github.com/tidymodels/parsnip/issues/671)).

- Updated the parsnip add-in with new models and engines.

- Updated parameter ranges for some
  [`tunable()`](https://generics.r-lib.org/reference/tunable.html)
  methods and added a missing engine argument for brulee models.

- Added information about how to install the mixOmics package for PLS
  models ([\#680](https://github.com/tidymodels/parsnip/issues/680))

## parsnip 0.2.0

CRAN release: 2022-03-09

### Model Specification Changes

- Bayesian additive regression trees (BART) were added via the
  [`bart()`](https://parsnip.tidymodels.org/dev/reference/bart.md)
  function.

- Added the `"glm"` engine for
  [`linear_reg()`](https://parsnip.tidymodels.org/dev/reference/linear_reg.md)
  for numeric outcomes
  ([\#624](https://github.com/tidymodels/parsnip/issues/624)).

- Added `brulee` engines for
  [`linear_reg()`](https://parsnip.tidymodels.org/dev/reference/linear_reg.md),
  [`logistic_reg()`](https://parsnip.tidymodels.org/dev/reference/logistic_reg.md),
  [`multinom_reg()`](https://parsnip.tidymodels.org/dev/reference/multinom_reg.md)
  and [`mlp()`](https://parsnip.tidymodels.org/dev/reference/mlp.md).

### Bug fixes

- A bug for class predictions of two-class GAM models was fixed
  ([\#541](https://github.com/tidymodels/parsnip/issues/541))

- Fixed a bug for
  [`logistic_reg()`](https://parsnip.tidymodels.org/dev/reference/logistic_reg.md)
  with the LiblineaR engine
  ([\#552](https://github.com/tidymodels/parsnip/issues/552)).

- The list column produced when creating survival probability
  predictions is now always called `.pred` (with `.pred_survival` being
  used inside of the list column).

- Fixed outcome type checking affecting a subset of regression models
  ([\#625](https://github.com/tidymodels/parsnip/issues/625)).

- Prediction using
  [`multinom_reg()`](https://parsnip.tidymodels.org/dev/reference/multinom_reg.md)
  with the `nnet` engine with a single row no longer fails
  ([\#612](https://github.com/tidymodels/parsnip/issues/612)).

### Other Changes

- When the xy interface is used and the underlying model expects to use
  a matrix, a better warning is issued when predictors contain
  non-numeric columns (including dates).

- The fit time is only calculated when the `verbosity` argument of
  [`control_parsnip()`](https://parsnip.tidymodels.org/dev/reference/control_parsnip.md)
  is 2L or greater. Also, the call to
  [`system.time()`](https://rdrr.io/r/base/system.time.html) now uses
  `gcFirst = FALSE`.
  ([\#611](https://github.com/tidymodels/parsnip/issues/611))

- [`fit_control()`](https://parsnip.tidymodels.org/dev/reference/fit_control.md)
  is soft-deprecated in favor of
  [`control_parsnip()`](https://parsnip.tidymodels.org/dev/reference/control_parsnip.md).

- New
  [`extract_parameter_set_dials()`](https://hardhat.tidymodels.org/reference/hardhat-extract.html)
  method to extract parameter sets from model specs.

- New
  [`extract_parameter_dials()`](https://hardhat.tidymodels.org/reference/hardhat-extract.html)
  method to extract a single parameter from model specs.

- Argument `interval` was added for prediction: For types `"survival"`
  and `"quantile"`, estimates for the confidence or prediction interval
  can be added if available
  ([\#615](https://github.com/tidymodels/parsnip/issues/615)).

- [`set_dependency()`](https://parsnip.tidymodels.org/dev/reference/set_new_model.md)
  now allows developers to create package requirements that are specific
  to the model’s mode
  ([\#604](https://github.com/tidymodels/parsnip/issues/604)).

- [`varying()`](https://parsnip.tidymodels.org/dev/reference/varying.md)
  is soft-deprecated in favor of
  [`tune()`](https://hardhat.tidymodels.org/reference/tune.html).

- [`varying_args()`](https://generics.r-lib.org/reference/varying_args.html)
  is soft-deprecated in favor of
  [`tune_args()`](https://generics.r-lib.org/reference/tune_args.html).

- An
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  method was added for glmnet objects, showing the coefficient paths
  versus the penalty values
  ([\#642](https://github.com/tidymodels/parsnip/issues/642)).

- parsnip is now more robust working with keras and tensorflow for a
  larger range of versions
  ([\#596](https://github.com/tidymodels/parsnip/issues/596)).

- xgboost engines now use the new `iterationrange` parameter instead of
  the deprecated `ntreelimit`
  ([\#656](https://github.com/tidymodels/parsnip/issues/656)).

### Developer

- Models information can be re-registered as long as the information
  being registered is the same. This is helpful for packages that add
  new engines and use `devtools::load_all()`
  ([\#653](https://github.com/tidymodels/parsnip/issues/653)).

## parsnip 0.1.7

CRAN release: 2021-07-21

### Model Specification Changes

- A model function
  ([`gen_additive_mod()`](https://parsnip.tidymodels.org/dev/reference/gen_additive_mod.md))
  was added for generalized additive models.

- Each model now has a default engine that is used when the model is
  defined. The default for each model is listed in the help documents.
  This also adds functionality to declare an engine in the model
  specification function.
  [`set_engine()`](https://parsnip.tidymodels.org/dev/reference/set_engine.md)
  is still required if engine-specific arguments need to be added.
  ([\#513](https://github.com/tidymodels/parsnip/issues/513))

- parsnip now checks for a valid combination of engine and mode
  ([\#529](https://github.com/tidymodels/parsnip/issues/529))

- The default engine for
  [`multinom_reg()`](https://parsnip.tidymodels.org/dev/reference/multinom_reg.md)
  was changed to `nnet`.

### Other Changes

- The helper functions
  [`.convert_form_to_xy_fit()`](https://parsnip.tidymodels.org/dev/reference/convert_helpers.md),
  [`.convert_form_to_xy_new()`](https://parsnip.tidymodels.org/dev/reference/convert_helpers.md),
  [`.convert_xy_to_form_fit()`](https://parsnip.tidymodels.org/dev/reference/convert_helpers.md),
  and
  [`.convert_xy_to_form_new()`](https://parsnip.tidymodels.org/dev/reference/convert_helpers.md)
  for converting between formula and matrix interface are now exported
  for developer use
  ([\#508](https://github.com/tidymodels/parsnip/issues/508)).

- Fix bug in
  [`augment()`](https://generics.r-lib.org/reference/augment.html) when
  non-predictor, non-outcome variables are included in data
  ([\#510](https://github.com/tidymodels/parsnip/issues/510)).

- New article “Fitting and Predicting with parsnip” which contains
  examples for various combinations of model type and engine. (
  [\#527](https://github.com/tidymodels/parsnip/issues/527))

## parsnip 0.1.6

CRAN release: 2021-05-27

### Model Specification Changes

- A new linear SVM model
  [`svm_linear()`](https://parsnip.tidymodels.org/dev/reference/svm_linear.md)
  is now available with the `LiblineaR` engine
  ([\#424](https://github.com/tidymodels/parsnip/issues/424)) and the
  `kernlab` engine
  ([\#438](https://github.com/tidymodels/parsnip/issues/438)), and the
  `LiblineaR` engine is available for
  [`logistic_reg()`](https://parsnip.tidymodels.org/dev/reference/logistic_reg.md)
  as well ([\#429](https://github.com/tidymodels/parsnip/issues/429)).
  These models can use sparse matrices via
  [`fit_xy()`](https://generics.r-lib.org/reference/fit_xy.html)
  ([\#447](https://github.com/tidymodels/parsnip/issues/447)) and have a
  `tidy` method
  ([\#474](https://github.com/tidymodels/parsnip/issues/474)).

- For models with `glmnet` engines:

  - A single value is required for `penalty` (either a single numeric
    value or a value of
    [`tune()`](https://hardhat.tidymodels.org/reference/tune.html))
    ([\#481](https://github.com/tidymodels/parsnip/issues/481)).
  - A special argument called `path_values` can be used to set the
    `lambda` path as a specific set of numbers (independent of the value
    of `penalty`). A pure ridge regression models (i.e., `mixture = 1`)
    will generate incorrect values if the path does not include zero.
    See issue [\#431](https://github.com/tidymodels/parsnip/issues/431)
    for discussion
    ([\#486](https://github.com/tidymodels/parsnip/issues/486)).

- The `liquidSVM` engine for
  [`svm_rbf()`](https://parsnip.tidymodels.org/dev/reference/svm_rbf.md)
  was deprecated due to that package’s removal from CRAN.
  ([\#425](https://github.com/tidymodels/parsnip/issues/425))

- The xgboost engine for boosted trees was translating `mtry` to
  xgboost’s `colsample_bytree`. We now map `mtry` to `colsample_bynode`
  since that is more consistent with how random forest works.
  `colsample_bytree` can still be optimized by passing it in as an
  engine argument. `colsample_bynode` was added to xgboost after the
  `parsnip` package code was written.
  ([\#495](https://github.com/tidymodels/parsnip/issues/495))

- For xgboost, `mtry` and `colsample_bytree` can be passed as integer
  counts or proportions, while `subsample` and `validation` should
  always be proportions.
  [`xgb_train()`](https://parsnip.tidymodels.org/dev/reference/xgb_train.md)
  now has a new option `counts` (`TRUE` or `FALSE`) that states which
  scale for `mtry` and `colsample_bytree` is being used.
  ([\#461](https://github.com/tidymodels/parsnip/issues/461))

### Other Changes

- Re-licensed package from GPL-2 to MIT. See [consent from copyright
  holders here](https://github.com/tidymodels/parsnip/issues/462).

- [`set_mode()`](https://parsnip.tidymodels.org/dev/reference/set_args.md)
  now checks if `mode` is compatible with the model class, similar to
  [`new_model_spec()`](https://parsnip.tidymodels.org/dev/reference/add_on_exports.md)
  ([@jtlandis](https://github.com/jtlandis),
  [\#467](https://github.com/tidymodels/parsnip/issues/467)). Both
  [`set_mode()`](https://parsnip.tidymodels.org/dev/reference/set_args.md)
  and
  [`set_engine()`](https://parsnip.tidymodels.org/dev/reference/set_engine.md)
  now error for `NULL` or missing arguments
  ([\#503](https://github.com/tidymodels/parsnip/issues/503)).

- Re-organized model documentation:

  - `update` methods were moved out of the model help files
    ([\#479](https://github.com/tidymodels/parsnip/issues/479)).
  - Each model/engine combination has its own help page.
  - The model help page has a dynamic bulleted list of the engines with
    links to the individual help pages.

- [`generics::required_pkgs()`](https://generics.r-lib.org/reference/required_pkgs.html)
  was extended for `parsnip` objects.

- Prediction functions now give a consistent error when a user uses an
  unavailable value of `type`
  ([\#489](https://github.com/tidymodels/parsnip/issues/489))

- The [`augment()`](https://generics.r-lib.org/reference/augment.html)
  method was changed to avoid failing if the model does not enable class
  probabilities. The method now returns tibbles despite the input data
  class ([\#487](https://github.com/tidymodels/parsnip/issues/487))
  ([\#478](https://github.com/tidymodels/parsnip/issues/478))

- xgboost engines now respect the `event_level` option for predictions
  ([\#460](https://github.com/tidymodels/parsnip/issues/460)).

## parsnip 0.1.5

CRAN release: 2021-01-19

- An RStudio add-in is available that makes writing multiple `parsnip`
  model specifications to the source window. It can be accessed via the
  IDE addin menus or by calling
  [`parsnip_addin()`](https://parsnip.tidymodels.org/dev/reference/parsnip_addin.md).

- For `xgboost` models, users can now pass `objective` to
  `set_engine("xgboost")`.
  ([\#403](https://github.com/tidymodels/parsnip/issues/403))

- Changes to test for cases when CRAN cannot get `xgboost` to work on
  their Solaris configuration.

- There is now an `augument()` method for fitted models. See
  `augment.model_fit`.
  ([\#401](https://github.com/tidymodels/parsnip/issues/401))

- Column names for `x` are now required when
  [`fit_xy()`](https://generics.r-lib.org/reference/fit_xy.html) is
  used. ([\#398](https://github.com/tidymodels/parsnip/issues/398))

- There is now an `event_level` argument for the `xgboost` engine.
  ([\#420](https://github.com/tidymodels/parsnip/issues/420))

- New mode “censored regression” and new prediction types “linear_pred”,
  “time”, “survival”, “hazard”.
  ([\#396](https://github.com/tidymodels/parsnip/issues/396))

- Censored regression models cannot use
  [`fit_xy()`](https://generics.r-lib.org/reference/fit_xy.html) (use
  [`fit()`](https://generics.r-lib.org/reference/fit.html)).
  ([\#442](https://github.com/tidymodels/parsnip/issues/442))

## parsnip 0.1.4

CRAN release: 2020-10-27

- [`show_engines()`](https://parsnip.tidymodels.org/dev/reference/show_engines.md)
  will provide information on the current set for a model.

- For three models (`glmnet`, `xgboost`, and `ranger`), enable sparse
  matrix use via
  [`fit_xy()`](https://generics.r-lib.org/reference/fit_xy.html)
  ([\#373](https://github.com/tidymodels/parsnip/issues/373)).

- Some added protections were added for function arguments that are
  dependent on the data dimensions (e.g., `mtry`, `neighbors`, `min_n`,
  etc). ([\#184](https://github.com/tidymodels/parsnip/issues/184))

- Infrastructure was improved for running `parsnip` models in parallel
  using PSOCK clusters on Windows.

## parsnip 0.1.3

CRAN release: 2020-08-04

- A [`glance()`](https://generics.r-lib.org/reference/glance.html)
  method for `model_fit` objects was added
  ([\#325](https://github.com/tidymodels/parsnip/issues/325))

- Specific [`tidy()`](https://generics.r-lib.org/reference/tidy.html)
  methods for `glmnet` models fit via `parsnip` were created so that the
  coefficients for the specific fitted `parsnip` model are returned.

### Fixes

- `glmnet` models were fitting two intercepts
  ([\#349](https://github.com/tidymodels/parsnip/issues/349))

- The various [`update()`](https://rdrr.io/r/stats/update.html) methods
  now work with engine-specific parameters.

## parsnip 0.1.2

CRAN release: 2020-07-03

### Breaking Changes

- `parsnip` now has options to set specific types of predictor encodings
  for different models. For example, `ranger` models run using `parsnip`
  and `workflows` do the same thing by *not* creating indicator
  variables. These encodings can be overridden using the `blueprint`
  options in `workflows`. As a consequence, it is possible to get a
  different model fit that previous versions of `parsnip`. More details
  about specific encoding changes are below.
  ([\#326](https://github.com/tidymodels/parsnip/issues/326))

### Other Changes

- `tidyr` \>= 1.0.0 is now required.

- SVM models produced by `kernlab` now use the formula method (see
  breaking change notice above). This change was due to how `ksvm()`
  made indicator variables for factor predictors (with one-hot
  encodings). Since the ordinary formula method did not do this, the
  data are passed as-is to `ksvm()` so that the results are closer to
  what one would get if `ksmv()` were called directly.

- MARS models produced by `earth` now use the formula method.

- For `xgboost`, a one-hot encoding is used when indicator variables are
  created.

- Under-the-hood changes were made so that non-standard data arguments
  in the modeling packages can be accommodated.
  ([\#315](https://github.com/tidymodels/parsnip/issues/315))

### New Features

- A new main argument was added to
  [`boost_tree()`](https://parsnip.tidymodels.org/dev/reference/boost_tree.md)
  called `stop_iter` for early stopping. The
  [`xgb_train()`](https://parsnip.tidymodels.org/dev/reference/xgb_train.md)
  function gained arguments for early stopping and a percentage of data
  to leave out for a validation set.

- If [`fit()`](https://generics.r-lib.org/reference/fit.html) is used
  and the underlying model uses a formula, the *actual* formula is pass
  to the model (instead of a placeholder). This makes the model call
  better.

- A function named
  [`repair_call()`](https://parsnip.tidymodels.org/dev/reference/repair_call.md)
  was added. This can help change the underlying models `call` object to
  better reflect what they would have obtained if the model function had
  been used directly (instead of via `parsnip`). This is only useful
  when the user chooses a formula interface and the model uses a formula
  interface. It will also be of limited use when a recipes is used to
  construct the feature set in `workflows` or `tune`.

- The [`predict()`](https://rdrr.io/r/stats/predict.html) function now
  checks to see if required modeling packages are installed. The
  packages are loaded (but not attached).
  ([\#249](https://github.com/tidymodels/parsnip/issues/249))
  ([\#308](https://github.com/tidymodels/parsnip/issues/308))
  (tidymodels/workflows#45)

- The function
  [`req_pkgs()`](https://parsnip.tidymodels.org/dev/reference/req_pkgs.md)
  is a user interface to determining the required packages.
  ([\#308](https://github.com/tidymodels/parsnip/issues/308))

## parsnip 0.1.1

CRAN release: 2020-05-06

### New Features

- `liquidSVM` was added as an engine for
  [`svm_rbf()`](https://parsnip.tidymodels.org/dev/reference/svm_rbf.md)
  ([\#300](https://github.com/tidymodels/parsnip/issues/300))

### Fixes

- The error message for missing packages was fixed
  ([\#289](https://github.com/tidymodels/parsnip/issues/289) and
  [\#292](https://github.com/tidymodels/parsnip/issues/292))

### Other Changes

- S3 dispatch for
  [`tidy()`](https://generics.r-lib.org/reference/tidy.html) was broken
  on R 4.0.

## parsnip 0.0.5

CRAN release: 2020-01-07

### Fixes

- A bug ([\#206](https://github.com/tidymodels/parsnip/issues/206) and
  [\#234](https://github.com/tidymodels/parsnip/issues/234)) was fixed
  that caused an error when predicting with a multinomial `glmnet`
  model.

### Other Changes

- `glmnet` was removed as a dependency since the new version depends on
  3.6.0 or greater. Keeping it would constrain `parsnip` to that same
  requirement. All `glmnet` tests are run locally.

- A set of internal functions are now exported. These are helpful when
  creating a new package that registers new model specifications.

### New Features

- `nnet` was added as an engine to
  [`multinom_reg()`](https://parsnip.tidymodels.org/dev/reference/multinom_reg.md)
  [\#209](https://github.com/tidymodels/parsnip/issues/209)

### Breaking Changes

- There were some mis-mapped parameters (going between `parsnip` and the
  underlying model function) for `spark` boosted trees and some `keras`
  models. See
  [897c927](https://github.com/tidymodels/parsnip/commit/897c92719332caf7344e7c9c8895ac673517d2c8).

## parsnip 0.0.4

CRAN release: 2019-11-02

### New Features

- The time elapsed during model fitting is stored in the `$elapsed` slot
  of the parsnip model object, and is printed when the model object is
  printed.

- Some default parameter ranges were updated for SVM, KNN, and MARS
  models.

- The model `udpate()` methods gained a `parameters` argument for cases
  when the parameters are contained in a tibble or list.

- [`fit_control()`](https://parsnip.tidymodels.org/dev/reference/fit_control.md)
  is soft-deprecated in favor of
  [`control_parsnip()`](https://parsnip.tidymodels.org/dev/reference/control_parsnip.md).

### Fixes

- [A bug](https://github.com/tidymodels/parsnip/issues/222) was fixed
  standardizing the output column types of `multi_predict` and `predict`
  for `multinom_reg`.

- [A bug](https://github.com/tidymodels/parsnip/issues/208) was fixed
  related to using data descriptors and
  [`fit_xy()`](https://generics.r-lib.org/reference/fit_xy.html).

- A bug was fixed related to the column names generated by
  [`multi_predict()`](https://parsnip.tidymodels.org/dev/reference/multi_predict.md).
  The top-level tibble will always have a column named `.pred` and this
  list column contains tibbles across sub-models. The column names for
  these sub-model tibbles will have names consistent with
  [`predict()`](https://rdrr.io/r/stats/predict.html) (which was
  previously incorrect). See
  [43c15db](https://github.com/tidymodels/parsnip/commit/43c15db377ea9ef27483ff209f6bd0e98cb830d2).

- [A bug](https://github.com/tidymodels/parsnip/issues/174) was fixed
  standardizing the column names of `nnet` class probability
  predictions.

## parsnip 0.0.3.1

CRAN release: 2019-08-06

Test case update due to CRAN running extra tests
[(](https://github.com/tidymodels/parsnip/issues/202)[\#202](https://github.com/tidymodels/parsnip/issues/202))

## parsnip 0.0.3

CRAN release: 2019-07-31

Unplanned release based on CRAN requirements for Solaris.

### Breaking Changes

- The method that `parsnip` stores the model information has changed.
  Any custom models from previous versions will need to use the new
  method for registering models. The methods are detailed in
  [`?get_model_env`](https://parsnip.tidymodels.org/dev/reference/get_model_env.md)
  and the [package vignette for adding
  models](https://parsnip.tidymodels.org/articles/articles/Scratch.html).

- The mode needs to be declared for models that can be used for more
  than one mode prior to fitting and/or translation.

- For
  [`surv_reg()`](https://parsnip.tidymodels.org/dev/reference/surv_reg.md),
  the engine that uses the `survival` package is now called `survival`
  instead of `survreg`.

- For `glmnet` models, the full regularization path is always fit
  regardless of the value given to `penalty`. Previously, the model was
  fit with passing `penalty` to `glmnet`’s `lambda` argument and the
  model could only make predictions at those specific values.
  [(](https://github.com/tidymodels/parsnip/issues/195)[\#195](https://github.com/tidymodels/parsnip/issues/195))

### New Features

- [`add_rowindex()`](https://parsnip.tidymodels.org/dev/reference/add_rowindex.md)
  can create a column called `.row` to a data frame.

- If a computational engine is not explicitly set, a default will be
  used. Each default is documented on the corresponding model page. A
  warning is issued at fit time unless verbosity is zero.

- [`nearest_neighbor()`](https://parsnip.tidymodels.org/dev/reference/nearest_neighbor.md)
  gained a `multi_predict` method. The
  [`multi_predict()`](https://parsnip.tidymodels.org/dev/reference/multi_predict.md)
  documentation is a little better organized.

- A suite of internal functions were added to help with upcoming model
  tuning features.

- A `parsnip` object always saved the name(s) of the outcome variable(s)
  for proper naming of the predicted values.

## parsnip 0.0.2

CRAN release: 2019-03-22

Small release driven by changes in
[`sample()`](https://rdrr.io/r/base/sample.html) in the current r-devel.

### New Features

- A “null model” is now available that fits a predictor-free model
  (using the mean of the outcome for regression or the mode for
  classification).

- [`fit_xy()`](https://generics.r-lib.org/reference/fit_xy.html) can
  take a single column data frame or matrix for `y` without error

### Other Changes

- [`varying_args()`](https://generics.r-lib.org/reference/varying_args.html)
  now has a `full` argument to control whether the full set of possible
  varying arguments is returned (as opposed to only the arguments that
  are actually varying).

- [`fit_control()`](https://parsnip.tidymodels.org/dev/reference/fit_control.md)
  not returns an S3 method.

- For classification models, an error occurs if the outcome data are not
  encoded as factors
  ([\#115](https://github.com/tidymodels/parsnip/issues/115)).

- The prediction modules (e.g. `predict_class`, `predict_numeric`, etc)
  were de-exported. These were internal functions that were not to be
  used by the users and the users were using them.

- An event time data set (`check_times`) was included that is the time
  (in seconds) to run `R CMD check` using the
  “r-devel-windows-ix86+x86_64\` flavor. Packages that errored are
  censored.

### Bug Fixes

- [`varying_args()`](https://generics.r-lib.org/reference/varying_args.html)
  now uses the version from the `generics` package. This means that the
  first argument, `x`, has been renamed to `object` to align with
  generics.

- For the recipes step method of
  [`varying_args()`](https://generics.r-lib.org/reference/varying_args.html),
  there is now error checking to catch if a user tries to specify an
  argument that *cannot* be varying as varying (for example, the `id`)
  ([\#132](https://github.com/tidymodels/parsnip/issues/132)).

- `find_varying()`, the internal function for detecting varying
  arguments, now returns correct results when a size 0 argument is
  provided. It can also now detect varying arguments nested deeply into
  a call ([\#131](https://github.com/tidymodels/parsnip/issues/131),
  [\#134](https://github.com/tidymodels/parsnip/issues/134)).

- For multinomial regression, the `.pred_` prefix is now only added to
  prediction column names once
  ([\#107](https://github.com/tidymodels/parsnip/issues/107)).

- For multinomial regression using glmnet,
  [`multi_predict()`](https://parsnip.tidymodels.org/dev/reference/multi_predict.md)
  now pulls the correct default penalty
  ([\#108](https://github.com/tidymodels/parsnip/issues/108)).

- Confidence and prediction intervals for logistic regression were only
  computed the intervals for a single level. Both are now computed.
  ([\#156](https://github.com/tidymodels/parsnip/issues/156))

## parsnip 0.0.1

CRAN release: 2018-11-12

First CRAN release

## parsnip 0.0.0.9005

- The engine, and any associated arguments, are now specified using
  [`set_engine()`](https://parsnip.tidymodels.org/dev/reference/set_engine.md).
  There is no `engine` argument

## parsnip 0.0.0.9004

- Arguments to modeling functions are now captured as quosures.
- `others` has been replaced by `...`
- Data descriptor names have been changed and are now functions. The
  descriptor definitions for “cols” and “preds” have been switched.

## parsnip 0.0.0.9003

- `regularization` was changed to `penalty` in a few models to be
  consistent with [this
  change](https://tidymodels.github.io/model-implementation-principles/standardized-argument-names.html#tuning-parameters).
- If a mode is not chosen in the model specification, it is assigned at
  the time of fit. [51](https://github.com/tidymodels/parsnip/issues/51)
- The underlying modeling packages now are loaded by namespace. There
  will be some exceptions noted in the documentation for each model. For
  example, in some `predict` methods, the `earth` package will need to
  be attached to be fully operational.

## parsnip 0.0.0.9002

- To be consistent with `snake_case`, `newdata` was changed to
  `new_data`.
- A `predict_raw` method was added.

## parsnip 0.0.0.9001

- A package dependency suffered a new change.

## parsnip 0.0.0.9000

- The `fit` interface was previously used to cover both the x/y
  interface as well as the formula interface. Now,
  [`fit()`](https://generics.r-lib.org/reference/fit.html) is the
  formula interface and [`fit_xy()` is for the x/y
  interface](https://github.com/tidymodels/parsnip/issues/33).
- Added a `NEWS.md` file to track changes to the package.
- `predict` methods were
  [overhauled](https://github.com/tidymodels/parsnip/issues/34) to be
  [consistent](https://github.com/tidymodels/parsnip/issues/41).
- MARS was added.
