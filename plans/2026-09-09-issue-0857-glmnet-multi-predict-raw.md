# Issue #857: Disable `type = "raw"` for `multi_predict()`

## Issue

- Number: #857
- Title: Disable `type = "raw"` for `multi_predict()`
- URL: https://github.com/tidymodels/parsnip/issues/857
- Filed: 2023-01-18
- Current labels: `feature`
- Classification: this is a bug, not a feature. `multi_predict()` on a glmnet
  `linear_reg()` silently ignores a user-supplied `type = "raw"` and returns
  regular nested numeric predictions, while the same call on `logistic_reg()`
  or `multinom_reg()` fails with an internal error that never mentions that
  `"raw"` is unsupported. Silently dropping a user argument and inconsistent,
  uninformative errors are unintended behavior. (The eventual resolution —
  wiring raw predictions through — has a feature flavor, which likely explains
  the label, but the current inconsistency is a defect.)

hfrick (maintainer) stated the direction in the thread: supporting
`type = "raw"` in `multi_predict()` is fine as long as parsnip does *no*
post-processing inside the glmnet `multi_predict()` method.

## Symptom

Reproduced on current `main` with `devtools::load_all()` (glmnet and modeldata
installed).

Linear regression: `type = "raw"` is silently ignored and the normal nested
tibble is returned.

```r
data("hpc_data", package = "modeldata")
hpc <- hpc_data[1:150, c(2:5, 8)]
lm_fit <- linear_reg(penalty = 0.123) |>
  set_engine("glmnet") |>
  fit(input_fields ~ log(compounds) + class, data = hpc)
multi_predict(lm_fit, hpc[1:5, ], type = "raw", penalty = c(0.1, 0.5))
#> # A tibble: 5 × 1
#>   .pred
#>   <list>
#> 1 <tibble [2 × 2]>
#> 2 <tibble [2 × 2]>
#> 3 <tibble [2 × 2]>
#> 4 <tibble [2 × 2]>
#> 5 <tibble [2 × 2]>
```

Logistic regression: errors, but from parsnip's internal formatting helper
(this has shifted since the issue was filed; back then the error came from
glmnet's `match.arg()`).

```r
data("lending_club", package = "modeldata")
lending_club <- lending_club[1:200, ]
lr_fit <- logistic_reg(penalty = 0.123) |>
  set_engine("glmnet") |>
  fit(Class ~ log(funded_amnt) + int_rate + term, data = lending_club)
multi_predict(lr_fit, lending_club[1:5, ], type = "raw", penalty = c(0.123, 0.5))
#> Error in `format_glmnet_multi_logistic_reg()`:
#> ! `type` must be one of "class" or "prob", not "raw".
```

Multinomial regression: errors from inside glmnet.

```r
mn_fit <- multinom_reg(penalty = 0.1) |>
  set_engine("glmnet") |>
  fit(class ~ ., data = hpc)
multi_predict(mn_fit, hpc[1:5, ], type = "raw", penalty = c(0.05, 0.1))
#> Error in match.arg(type) :
#>   'arg' should be one of "link", "response", "coefficients", "class", "nonzero"
```

## Root cause

All four glmnet `multi_predict()` methods (`._elnet`, `._lognet`, `._multnet`,
`._glmnetfit`) share `multi_predict_glmnet()` in `R/glmnet-engines.R:185-258`.

- `R/glmnet-engines.R:192-193`: `check_pred_type()` and
  `check_spec_pred_type()` both accept `"raw"` (raw is a registered prediction
  module for all glmnet engines), so `type = "raw"` sails through validation.

- `R/glmnet-engines.R:219-228`: for classification models, the glmnet-level
  prediction type is set in `dots$type`. For `logistic_reg`, any type
  (including `"raw"`) is mapped to `"response"`; for `multinom_reg` with
  `type = "raw"`, `dots$type <- "raw"` is passed straight to
  `predict.multnet()`, whose `match.arg(type)` rejects it — that is the glmnet
  error above.

- `R/glmnet-engines.R:230-237`: the actual prediction is *always* made with
  `predict(object, type = "raw", ...)` internally, regardless of the
  user-facing `type`.

- `R/glmnet-engines.R:239-255`: the raw prediction is then unconditionally
  post-processed by `format_glmnet_multi_linear_reg()` (which ignores `type`
  entirely — hence the silent behavior for `linear_reg`),
  `format_glmnet_multi_logistic_reg()` (`rlang::arg_match(type,
  c("class", "prob"))` at line 296 — hence the arg_match error), or
  `format_glmnet_multi_multinom_reg()` (same `arg_match` at line 330, never
  reached because glmnet errors first).

So `type = "raw"` is never honored: it is either overwritten by the formatting
step (regression) or trips validation in parsnip or glmnet (classification).

## Proposed fix

Follow hfrick's stated direction: support `type = "raw"` consistently for all
glmnet models by passing it through with *no* post-processing. This matches
the semantics of `predict(object, type = "raw")`, where
`format_predictions(x, "raw")` (`R/predict.R:307`) already returns the engine
output untouched.

In `multi_predict_glmnet()` (`R/glmnet-engines.R:185-258`):

- Leave the existing `check_pred_type()`/`check_spec_pred_type()` calls as-is
  (they already permit `"raw"`).

- Do not set `dots$type` when `type == "raw"`; only apply the existing
  classification mapping (`R/glmnet-engines.R:219-228`) for `"class"` and
  `"prob"`. With `type == "raw"`, `dots` (the user's `...`) is passed through
  as `opts`, so glmnet uses its own default (`type = "link"`), exactly as a
  plain `predict(fit, type = "raw")` with empty `opts` does.

- After the internal `predict(..., type = "raw", multi = TRUE)` call
  (`R/glmnet-engines.R:230-237`), return `pred` immediately when
  `type == "raw"`, skipping the `switch()` over the `format_glmnet_multi_*()`
  helpers.

Returned structure (document in the `multi_predict()` roxygen `@return`, noting
that `type = "raw"` returns the glmnet prediction object without any parsnip
post-processing and not the usual `.pred` list-column tibble):

- `linear_reg`/`logistic_reg` (elnet/lognet/glmnetfit): a numeric matrix with
  `nrow(new_data)` rows and one column per `penalty` value (glmnet's `s`
  columns), on the link scale by default.

- `multinom_reg` (multnet): glmnet's native output for the requested glmnet
  type, e.g. a 3-dimensional array (observations x levels x penalties) for
  `"link"`/`"response"`.

The penalty vector already flows correctly for raw: `predict_glmnet()`
validates it via `.check_glmnet_penalty_predict(multi = TRUE)`
(`R/glmnet-engines.R:42-46`) and `predict_raw_glmnet()` injects it as `opts$s`
(`R/glmnet-engines.R:70`).

Alternative (the issue's original title): error informatively for
`type = "raw"` in `multi_predict_glmnet()` with `cli::cli_abort()`. This is
simpler and consistent, but hfrick's later comment supersedes the title, so
the pass-through is the recommended option.

Interaction with #878: that issue plans to remove the `predict_<type>_glmnet()`
wrappers and the `eval_args()` calls. The fix here should stay inside
`multi_predict_glmnet()` and keep using the public `predict(type = "raw")`
entry point so that the #878 refactor does not have to special-case it. If
#878 lands first, revisit where `opts$s` is set (currently
`predict_raw_glmnet()`, which also silently overrides a user-supplied
`opts$s`).

## Tests

Source file is `R/glmnet-engines.R`, so tests go in a new
`tests/testthat/test-glmnet-engines.R` (no such file exists yet; deeper glmnet
coverage lives in tidymodels/extratests). Note that glmnet is not in
`Suggests`, so every test needs `skip_if_not_installed("glmnet")` (and
`skip_if_not_installed("modeldata")` where modeldata is used), following the
precedent in `tests/testthat/test-linear_reg.R`.

- `multi_predict()` with `type = "raw"` for a glmnet `linear_reg()`: check the
  result is a matrix with `expect_s3_class()`/dim expectations (a plain
  matrix, so check dimensions and use `expect_identical()` against
  `predict(fit$fit, ..., s = penalty)` output; do not nest-check `.pred`).

- Same for `logistic_reg()` (matrix) and `multinom_reg()` (array), checking
  dimensions against `length(penalty)` and `nrow(new_data)`.

- Regression guard: `multi_predict()` with `type = "class"`, `"prob"`, and
  `"numeric"` still returns the nested `.pred` tibble; use `expect_named()`
  on the outer tibble and on an unnested element rather than `expect_equal()`
  on names.

- If the abort alternative is chosen instead, use
  `expect_snapshot(error = TRUE)` for all three model types.

Mirror the coverage in extratests, which is where the existing glmnet
`multi_predict()` behavior is exercised on CI.

## Follow-ups / risks

Draft NEWS.md bullet:

- `multi_predict()` for glmnet engine fits now passes `type = "raw"` through to glmnet without post-processing, instead of silently ignoring it (linear regression) or erroring unhelpfully (logistic and multinomial regression) (#857).

Risks and related work:

- Behavior change: `multi_predict(type = "raw")` on `linear_reg()` glmnet fits
  currently returns the nested `.pred` tibble; after the fix it returns a
  matrix. Anyone relying on the (buggy) silent behavior will see a different
  structure.

- `multi_predict()` documents its return value as a tibble with a `.pred`
  list-column (`R/aaa_multi_predict.R:12-16`); the raw pass-through needs an
  explicit carve-out in that documentation and a re-document.

- Users cannot pass a glmnet-level `type` through `multi_predict()`'s `...`
  because it collides with the method's named `type` argument; raw predictions
  therefore always use glmnet's default (`"link"`). Document this.

- The issue notes the inconsistency "should probably apply to all
  models/engines"; other engines' `multi_predict()` methods (xgboost, C5.0,
  earth, kknn, nnet, brulee) have the same ambiguity and could get the same
  treatment in a follow-up issue — out of scope here.

- Coordinate with #878 (removal of `predict_<type>_glmnet()` wrappers and
  `eval_args()`): the internal raw call stack this fix relies on is exactly
  what #878 reshuffles.
