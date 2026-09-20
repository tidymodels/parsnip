# Issue #1406: `multi_predict()` for kknn errors when `neighbors` isn't supplied

## Issue

- Number: #1406
- Title: `multi_predict()` for kknn errors when `neighbors` isn't supplied
- URL: https://github.com/tidymodels/parsnip/issues/1406
- Filed: 2026-09-01 (no comments on the thread)
- Current labels: `tidy-dev-day :nerd_face:`
- Classification: a bug; it should arguably also carry the `bug` label. The documented default path (`neighbors = NULL`) is unusable for every kknn fit made through parsnip.

## Symptom

Reproduced live against the current checkout (kknn installed):

```r
devtools::load_all("/Users/max/github/parsnip")
fit <- nearest_neighbor(neighbors = 7) |>
  set_engine("kknn") |>
  set_mode("regression") |>
  fit(mpg ~ ., data = mtcars)

multi_predict(fit, new_data = mtcars[1:3, ])
#> Error in if (num_rows > n - offset) { : argument is of length zero
```

Inspecting the pieces confirms the mechanism:

```r
fit$fit$call$ks
#> min_rows(7, data, 5)                  # unevaluated expression, not a number

rlang::eval_tidy(fit$fit$call$ks)
#> Error in if (num_rows > n - offset) { : argument is of length zero

fit$fit$best.parameters
#> $kernel
#> [1] "optimal"
#> $k
#> [1] 7                                 # the actually fitted k
```

## Root cause

`/Users/max/github/parsnip/R/nearest_neighbor.R:159-164`, in `multi_predict._train.kknn()`:

```r
if (is.null(neighbors)) {
  neighbors <- rlang::eval_tidy(object$fit$call$ks)
}
```

At fit time, `translate.nearest_neighbor()` (`/Users/max/github/parsnip/R/nearest_neighbor.R:142-145`) wraps `ks` in a data-dimension guard: `rlang::call2("min_rows", rlang::eval_tidy(arg_vals$ks), expr(data), 5)`. The engine call recorded in `object$fit$call$ks` is therefore the unevaluated expression `min_rows(7, data, 5)`, referencing the fit-time `data` object.

When `multi_predict()` later evaluates that expression with no data mask, the symbol `data` resolves to the `utils::data` *function* from the search path. Inside `min_rows()` (`/Users/max/github/parsnip/R/arguments.R:371`), `n <- nrow(source)` is then `nrow(<function>)`, which is `NULL`, so `if (num_rows > n - offset)` compares against a zero-length value — the observed "argument is of length zero" error. In other environments the expression could just as easily evaluate to garbage instead of erroring; either way the fitted `k` is never recovered from the call.

## Proposed fix

Read the fitted value off the `train.kknn` object instead of re-evaluating the stale call, in `/Users/max/github/parsnip/R/nearest_neighbor.R:161-163`:

```r
if (is.null(neighbors)) {
  neighbors <- object$fit$best.parameters$k
}
```

This accessor was verified live: `fit$fit$best.parameters$k` is the integer `7`, and `multi_predict(fit, new_data = mtcars[1:3, ], neighbors = fit$fit$best.parameters$k)` returns the expected tibble of 1-row `.pred` tibbles with `neighbors == 7`, matching `predict()` (21.4, 21.3, 26.4 on `mtcars[1:3, ]`).

Why this is the right accessor:

- parsnip always passes a scalar `ks` (defaulted to 5 in `translate.nearest_neighbor()` at lines 135-137), so `kknn::train.kknn()`'s "best k over the tested `ks`" is exactly the fitted k.
- It reflects what was *actually* used: `min_rows()` caps `ks` at `n - 5` at fit time (e.g. `neighbors = 1000` on 333 rows fits with k = 328), and `best.parameters$k` records the capped value, whereas re-evaluating the call, even correctly, would redo the capping against the wrong data. The existing test at `/Users/max/github/parsnip/tests/testthat/test-nearest_neighbor_kknn.R:219-220` already treats `extract_fit_engine(fit)$best.parameters$k` as the source of truth for the fitted k.
- The downstream helper `knn_by_k()` (lines 190-196) already operates by assigning `object$fit$best.parameters$k <- k` before predicting, so the fallback and the prediction mechanism use the same slot.

Also update the `@param neighbors` roxygen text on `multi_predict._train.kknn()` (line 157) to state that `NULL` means "the value used to fit the model", and re-document.

## Tests

Add to `/Users/max/github/parsnip/tests/testthat/test-nearest_neighbor_kknn.R`, next to the existing 'kknn multi-predict' test (line 110), guarded by `skip_if_not_installed("kknn")`:

- Fit `nearest_neighbor(neighbors = 3)` (regression, mtcars, matching the existing test setup) and call `multi_predict(res, mtcars[cars_te, -1])` with no `neighbors`.
- `expect_s3_class()` the result as a tibble, `expect_named(unnested, c("neighbors", ".pred"))` after `tidyr::unnest(cols = c(.pred))`.
- `expect_all_equal(unnested$neighbors, 3)` (per convention, not `expect_true(all(...))`).
- Compare the unnested `.pred` to `predict(res, ...)$.pred` with `expect_equal()`.
- Optionally the same for a classification fit with `type = "prob"` left `NULL`, exercising the type-defaulting branch alongside the neighbors fallback.

No snapshots are needed (the fixed path returns a value; nothing errors or warns).

## Follow-ups / risks

- Draft NEWS.md bullet: `multi_predict()` for `nearest_neighbor()` models fit with the kknn engine now uses the number of neighbors from the fitted model when `neighbors` is not supplied, instead of erroring (#1406).
- Behavior change: none for working code — the `neighbors = NULL` path currently always errors for parsnip-created fits, so this only turns a failure into the documented behavior.
- Edge case, out of scope: a user who sets a vector via `set_engine("kknn", ks = c(3, 5))` bypasses parsnip's scalar assumption; `best.parameters$k` would return the single best k, which is a reasonable fallback, but the `min_rows()` wrapping in `translate.nearest_neighbor()` mishandles vector `ks` at fit time anyway (a separate latent issue).
- Related pattern: other engines that wrap args in `min_rows()` (`rand_forest`/ranger and randomForest, `decision_tree`/rpart, `boost_tree`) do not re-evaluate their recorded calls at predict time, so no analogous fix is needed there.
- No blockers; the fix is two lines plus a doc tweak.
