# Issues #1256 and #1258: `check_pred_type_dots()` error interpolation and dead deprecation path for `quantile`

## Issue

Two tightly coupled bugs in `check_pred_type_dots()` (R/predict.R), fixed together.

- Number: #1256

- Title: `check_pred_type_dots()` needs to evaluate `bad_args` in error message

- URL: https://github.com/tidymodels/parsnip/issues/1256

- Filed: 2025-02-13 by hfrick (maintainer); no comments

- Labels: `tidy-dev-day :nerd_face:` only. This is a straightforward bug (broken message interpolation); a `bug` label is missing but the dev-day label is reasonable given the small scope.

- Number: #1258

- Title: Deprecation of `quantile` arg to `predict_quantile()` not reached via `predict(type = "quantile")`

- URL: https://github.com/tidymodels/parsnip/issues/1258

- Filed: 2025-02-13 by hfrick (maintainer); no comments

- Labels: `tidy-dev-day :nerd_face:` only. Also a bug: the `lifecycle::deprecate_warn()` path for the old `quantile` argument is dead code because `check_pred_type_dots()` hard-errors first.

## Symptom

Repro for #1256 (run against current main with `devtools::load_all()`; output confirmed live):

```r
linear_reg() |>
  fit(mpg ~ ., data = mtcars) |>
  predict(type = "quantile", new_data = mtcars, quantile = c(0.2, 0.8))
#> Error in `predict()`:
#> ! The ellipses are not used to pass args to the model function's predict
#> function. These arguments cannot be used: "bad_args"
```

The literal string `"bad_args"` is printed instead of the offending argument names.

Repro for #1258 (also confirmed live; needs the censored package, which is installed here):

```r
library(censored)
f_fit <- survival_reg() |>
  fit(Surv(time, status) ~ age + ph.ecog, data = lung)
predict(f_fit, new_data = lung, type = "quantile", quantile = c(0.2, 0.8))
#> Error in `predict()`:
#> ! The ellipses are not used to pass args to the model function's predict
#> function. These arguments cannot be used: "bad_args"
```

The expected behavior is a deprecation warning pointing to `quantile_levels`, then successful prediction. Calling `predict_quantile(f_fit, lung, quantile = c(0.2, 0.8))` directly (bypassing `check_pred_type_dots()`) was confirmed live to fire the lifecycle warning ("Please use the `quantile_levels` argument instead.") and return predictions, proving the deprecation path works once the argument gets through.

## Root cause

Both bugs are in `check_pred_type_dots()`.

- #1256 — R/predict.R:511-517: `bad_args` is built at lines 511-512 (already collapsed into a single backtick-wrapped string), but the `cli::cli_abort()` call at line 513-517 uses `{.val bad_args}`, which cli treats as the literal value `"bad_args"` rather than interpolating the variable. Correct cli syntax needs a nested brace, e.g. `{.arg {bad_args}}` on the raw character vector, or plain `{bad_args}` on the pre-formatted string.

- #1258 — R/predict.R:496-505: the `other_args` allowlist contains `"quantile_levels"` but not `"quantile"`, so an old-style `quantile` argument in `...` is rejected here, in `predict.model_fit()` (via the call at R/predict.R:185), before dispatch ever reaches `predict_quantile.model_fit()`. The `lifecycle::deprecate_warn()` branch at R/predict_quantile.R:24-31 (deprecated in parsnip 1.3.0) is therefore unreachable from `predict()`, and users get the (broken, per #1256) hard error instead of a soft deprecation.

## Proposed fix

Both changes in `check_pred_type_dots()` in R/predict.R.

1. Add `"quantile"` to the `other_args` vector at lines 496-505, with a brief comment that it is only allowed through so that the `lifecycle::deprecate_warn()` in `predict_quantile.model_fit()` can fire, and should be removed when that deprecation is bumped to defunct.

2. Fix the message interpolation. Drop the manual backtick-wrapping at line 512 and let cli format and collapse the vector:

```r
is_pred_arg <- names(the_dots) %in% other_args
if (!all(is_pred_arg)) {
  bad_args <- names(the_dots)[!is_pred_arg]
  cli::cli_abort(
    "The ellipses are not used to pass args to the model function's
     predict function. {cli::qty(bad_args)}{?This argument/These arguments}
     cannot be used: {.arg {bad_args}}.",
    call = call
  )
}
```

(Pluralization is optional; the minimal fix is keeping the current wording and using `{.arg {bad_args}}`. Since `bad_args` is no longer pre-collapsed, line 512 must be deleted either way.)

No maintainer disagreement exists in either thread (no comments), so no alternatives are needed. Note the downstream behavior after the fix:

- The #1256 repro (lm, regression mode) will now fail later in `predict_quantile.model_fit()` at `check_spec_pred_type()` with the informative "No \"quantile\" prediction method available for this model" error — correct, since lm has no quantile prediction.

- The #1258 repro will warn about the deprecation and return predictions.

- Passing `quantile` with a non-quantile `type` (e.g. `type = "numeric"`) will now pass `check_pred_type_dots()` and instead fail at `check_dots_empty()` inside the type-specific predict function. Acceptable, and consistent with how other allowlisted args behave.

Check with `grep` confirmed no existing snapshot in tests/testthat/_snaps/ contains the "ellipses"/"cannot be used" message, so no existing snapshots need updating.

## Tests

The changed function lives in R/predict.R, so tests go in tests/testthat/test-predict.R (new file; no test file currently exists for R/predict.R).

- #1256: `expect_snapshot(error = TRUE)` on a `predict()` call with one and with two genuinely unknown dots args (e.g. `predict(lm_fit, mtcars, type = "numeric", perplexity = 3)`), verifying the actual argument names appear in the message.

- #1258: use a dependency-free quantile-capable model, e.g. `null_model() |> set_mode("quantile regression", quantile_levels = c(0.2, 0.8)) |> fit(...)`, then `expect_snapshot()` around `predict(fit, new_data, type = "quantile", quantile = c(0.2, 0.8))` capturing the lifecycle warning (in quantile regression mode this then errors because `quantile_levels` come from `set_mode()`, so use `expect_snapshot(error = TRUE)` and the snapshot will show both the warning and the error — which is exactly the evidence that the argument now gets through and the deprecation fires). Alternatively (or additionally) test the helper directly: `expect_no_error(parsnip:::check_pred_type_dots(fit, "quantile", quantile = c(0.2, 0.8)))`.

- Guard against lifecycle's once-per-session throttling with `withr::local_options(lifecycle_verbosity = "warning")` if the snapshot proves flaky (testthat usually forces warnings on).

## Follow-ups / risks

Draft NEWS.md bullets (two, one per issue):

- `predict()` now lists the actual offending argument names when unusable arguments are passed through `...`, instead of the literal string "bad_args" (#1256).

- `predict()` with `type = "quantile"` now triggers the deprecation warning for the old `quantile` argument (deprecated in favor of `quantile_levels` in parsnip 1.3.0) instead of erroring before the deprecation could fire (#1258).

Risks and related items:

- Behavior change: previously hard-erroring old-style `quantile` calls will now warn and succeed (for models supporting quantile prediction outside quantile-regression mode, e.g. censored's `survival_reg()`). This is the intended deprecation behavior, not a regression.

- The deprecation message says `predict_quantile(quantile)` even when the user called `predict()`; cosmetic, could be left as is.

- When the `quantile` deprecation is escalated (defunct), remember to remove `"quantile"` from `other_args` again — the code comment proposed above flags this.

- extension packages (censored) exercise this path; a revdep spot-check of censored's quantile prediction tests is cheap insurance.
