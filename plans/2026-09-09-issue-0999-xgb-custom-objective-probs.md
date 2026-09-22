# Issue #999: xgboost custom objective returns raw margins as probabilities

## Issue

- Number: #999
- Title: Xgboost objective function customizing error.
- URL: https://github.com/tidymodels/parsnip/issues/999
- Filed: 2023-09-09 (open); continuation of #873 and #875
- Current labels: `feature`
- Classification: mislabeled. Silently returning values outside [0, 1] from `predict(type = "prob")` (and class predictions thresholded at the wrong point) for a supported engine argument is a bug, not a feature request. The `feature` label fits only the deferred long-term work (embedding a user prediction post-processor into the spec, which simonpcouch linked to the post-processing infrastructure effort). simonpcouch confirmed the behavior and that xgboost itself cannot distinguish margins from probabilities under a custom objective; the reporter worked around it by registering a custom `xgboost_custom` engine whose `xgb_predict` variant applies the matching inverse link.

## Symptom

When a custom (function-valued) `objective` is passed via `set_engine("xgboost", objective = <function>)` for classification, `predict(type = "prob")` returns raw margins labeled as probabilities, and `predict(type = "class")` thresholds those margins at 0.5 (instead of 0), silently corrupting downstream metrics.

Minimal live repro (run against this checkout with xgboost 3.2.1.1):

```r
devtools::load_all('/Users/max/github/parsnip')
set.seed(6203)
n <- 300
x <- runif(n)
prob_yes <- 1 / (1 + exp(-(8 * x - 4)))
dat <- data.frame(
  x = x,
  cls = factor(ifelse(runif(n) < prob_yes, 'yes', 'no'), levels = c('no', 'yes'))
)
logregobj <- function(preds, dtrain) {
  labels <- xgboost::getinfo(dtrain, 'label')
  preds <- 1 / (1 + exp(-preds))
  list(grad = preds - labels, hess = preds * (1 - preds))
}
spec <- boost_tree(trees = 10) |>
  set_mode('classification') |>
  set_engine('xgboost', objective = logregobj)
fitted <- fit(spec, cls ~ x, data = dat)
class(fitted$fit$params$objective)
predict(fitted, head(dat), type = 'prob')
```

Observed output:

```
class of stored objective: NULL
# A tibble: 6 x 2
  .pred_no .pred_yes
     <dbl>     <dbl>
1  -1.59       2.59
2   3.10      -2.10
3   3.10      -2.10
4  -1.16       2.16
5   0.0425     0.957
6   1.20      -0.205
```

`type = "class"` also succeeds silently (the 0.5 cutoff lands on the raw log-odds scale). No error or warning anywhere.

## Root cause

- `R/boost_tree.R:483-503` (`xgb_predict()`): predictions are post-processed via `switch(object$params$objective %||% 3L, "binary:logitraw" = ..., "multi:softprob" = ..., res)`. Only those two objectives are recognized; everything else falls through to the default branch and returns `res` unchanged. (`"binary:logistic"` needs no branch because xgboost already returns probabilities for it.)
- Verified: the `switch()` never actually receives a function value, so it does not error. With current xgboost (>= 2.0; observed on 3.2.1.1), a function objective supplied to `xgb.train()` is not stored in the booster's `$params$objective` — it comes back `NULL` (see repro output above) — so `%||% 3L` routes to the default branch and raw margins pass through. The same held in the reporter's xgboost 1.x output.
- `R/boost_tree_data.R:182-205` (class `post`) and `R/boost_tree_data.R:207-231` (prob `post`) for the xgboost classification engine then treat the vector as a probability of the event level: the prob `post` builds `tibble(v1 = x, v2 = 1 - x)` and the class `post` applies `ifelse(x >= 0.5, ...)`. Neither checks that `x` is on [0, 1].
- How the objective gets in: in `xgb_train()`, a user objective arrives via `...` into `others` (`R/boost_tree.R:276`) and, for xgboost >= 2.0, is moved into `arg_list`/`params` at `R/boost_tree.R:355-362` because `objective` matches `xgboost::xgb.params` formals. Since it is non-`NULL`, the default-objective fallback at lines 364-374 is skipped.

xgboost cannot report whether a custom objective's output is a margin or a probability, so parsnip has no general way to convert — the tractable fix is to refuse to fabricate probabilities.

## Proposed fix

Detect a function-valued `objective` and error informatively for classification `prob` and `class` predictions, leaving `type = "raw"` (and regression) working.

- Add a small helper in `R/boost_tree.R`, e.g. `check_xgb_supported_objective(object)`: evaluate `object$spec$eng_args$objective` with `rlang::eval_tidy()` (engine args are quosures); if it is a function, `cli::cli_abort()` with class `"xgboost_custom_objective_error"` explaining that parsnip cannot convert raw margins from a custom objective into class probabilities, and pointing to the workarounds: use `predict(type = "raw")` and apply the matching inverse link manually, or register a custom engine with its own prediction post-processing (as the reporter did in the issue thread).
- Call the helper at the top of the classification `post` functions for `type = "class"` (`R/boost_tree_data.R:182-205`) and `type = "prob"` (`R/boost_tree_data.R:207-231`). Placing the guard in `post` (not `pre`) is deliberate: `xgb_by_tree()` (`R/boost_tree.R:678-685`) calls `object$spec$method$pred$class$post()` / `...$prob$post()` directly for `multi_predict()`, so a `post` guard covers both `predict()` and `multi_predict()` with one check. The `raw` registration (`R/boost_tree_data.R:233`) has `post = NULL` and stays untouched.
- Do not key the detection off `object$fit$params$objective` being `NULL`: that is an xgboost storage detail (and version-dependent); the spec's engine args are the reliable source.

Alternative (simpler, but discussed trade-off): error at fit time in `xgb_train()` when `y` is a factor and the objective is a function. This catches the problem earlier but blocks the legitimate fit-then-`type = "raw"` workflow and reverses the intent of #873/#875, which deliberately made function objectives passable; the existing test at `tests/testthat/test-boost_tree_xgboost.R:240` ("xgboost alternate objective") explicitly asserts that such a fit succeeds. Predict-time erroring is recommended.

Long term (out of scope, per simonpcouch): let users attach a prediction post-processor to the spec via the post-processing infrastructure, so custom objectives can produce real probabilities.

## Tests

Update and extend `tests/testthat/test-boost_tree_xgboost.R`, test `'xgboost alternate objective'` (line 240):

- Keep the regression custom/alternate objective assertions as is (lines 246-254).
- The classification block (lines 264-272) currently asserts `expect_no_error(predict(...))` for the default (class) type with a function objective — that asserts the buggy behavior. Replace it with `expect_snapshot(error = TRUE)` for both `predict(type = "class")` and `predict(type = "prob")` (snapshots go in `tests/testthat/_snaps/boost_tree_xgboost.md`).
- Add: fitting with a function objective still succeeds without condition, `predict(type = "raw")` still returns a numeric vector, and `multi_predict()` with `type = "class"` also produces the error (snapshot).
- Keep `skip_if_not_installed("xgboost")` and `skip_on_cran()` as in the existing test.

## Follow-ups / risks

- Draft NEWS bullet: `predict()` for `boost_tree()` models fit with the xgboost engine and a custom (function) `objective` now errors informatively for class and probability predictions instead of silently returning raw margins as probabilities; use `type = "raw"` or a custom engine to post-process such predictions (#999).
- Behavior change: code that fit classification models with function objectives and called `predict()` on them previously "worked" (with wrong values); it will now error. Given the outputs were invalid, this is the safe direction, but it must be flagged in NEWS.
- The existing test at `tests/testthat/test-boost_tree_xgboost.R:269-272` encodes the old behavior and must be updated (see above); check whether extension packages or tune snapshots exercise the same path.
- Non-function but unrecognized string objectives (e.g., `objective = "binary:hinge"`, which returns 0/1) still flow through the prob/class post-processing unchecked; a broader allowlist could be a follow-up, kept out of scope here to limit behavior changes.
- Related issues: #873, #875 (made custom objectives passable), and the tidymodels post-processing infrastructure work for the long-term fix.

## Work items

Executed 2026-09-22 on branch `xgboost-obj-function` (off `main` at b6c64a21, after #796 merged as #1430).

- [x] Reproduce raw margins being returned as probabilities
- [x] Add `check_xgb_supported_objective()` in `R/boost_tree.R`
- [x] Call it from the `class` and `prob` post-processors in `R/boost_tree_data.R`
- [x] Confirm `type = "raw"`, regression, and string objectives are unaffected
- [x] Confirm `multi_predict()` is covered by the same guard
- [x] Replace the test assertions that encoded the old behavior
- [x] `NEWS.md` bullet
- [x] `air format .` and full `R CMD check`

### Verification matrix

```
prob            ERRORS
class           ERRORS
raw             OK      (margins: -1.594 3.101 3.101 -1.16 0.043 1.205)
mp class        ERRORS
mp prob         ERRORS
default prob    OK
default class   OK
reg custom obj  OK
```

The plan's reasoning for guarding in `post` rather than `pre` is confirmed: `xgb_by_tree()` calls the `post` functions directly, so `multi_predict()` is covered by the same two call sites with no extra work.

Detection reads `object$spec$eng_args$objective` via `rlang::eval_tidy()`, not `object$fit$params$objective`. Confirmed necessary — xgboost does not store a function objective on the booster at all (it comes back `NULL` on 3.2.1.1), which is exactly why `switch(object$params$objective %||% 3L, ...)` silently fell through to the default branch in the first place.

### Error call

`cli::cli_abort(call = NULL)` rather than `call = caller_env()`. The caller is an anonymous `post` function, so the default rendered as ``Error in `object$spec$method$pred$class$post()` ``, which leaks an internal path and tells the user nothing. `call = NULL` renders a plain `Error:` followed by the message. Naming `predict()` explicitly was rejected because the same guard fires from `multi_predict()`.

### Test changes

`tests/testthat/test-boost_tree_xgboost.R`, `'xgboost alternate objective'`: the two assertions at the end (`expect_no_error(predict(...))` plus `expect_s3_class(..., "data.frame")`) encoded the buggy behavior and were replaced with error snapshots for `class` and `prob`, a class-based `expect_error()` for `multi_predict()`, and positive assertions that the fit still succeeds and `type = "raw"` still returns three doubles. The regression assertions earlier in the test are untouched.

`multi_predict()` uses `expect_error(class = "xgboost_custom_objective_error")` rather than a snapshot, deliberately: purrr wraps the failure with an `In index: 1` prefix and a `Caused by error in ... at parsnip/R/boost_tree.R:NNN` line, so a snapshot would bake in a source line number and churn on every unrelated edit to the file. The condition class is the stable contract.

### Snapshot hazard, avoided this time

The `snapshot_accept()` trap recorded in the #796 plan applies to this same file. Avoided by letting the ordinary test run write the brand-new snapshots directly into `_snaps/boost_tree_xgboost.md` and never calling `snapshot_accept()`. Verified the diff is 20 insertions and 0 deletions, with the skipped `xgboost execution, quantile regression` block intact.

### Still open

Non-function but unrecognised string objectives (for example `objective = "binary:hinge"`, which returns 0/1) still flow through the prob and class post-processing unchecked. Out of scope here, as the plan set out; an allowlist would be a broader behavior change.
