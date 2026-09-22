# Issue #1408: `predict_raw()` errors whenever `opts` contains a protected argument name

## Issue

- Number: #1408

- Title: `predict_raw()` errors whenever `opts` contains a protected argument name

- URL: https://github.com/tidymodels/parsnip/issues/1408

- Filed: 2026-09-01 by EmilHvitfeldt (maintainer); no comments

- Labels: `tidy-dev-day :nerd_face:` only. This is a bug (wrong subsetting operator turns an intended silent drop into a hard error); a `bug` label is missing but the classification in the issue body is accurate.

## Symptom

Repro (run against current main with `devtools::load_all()`; output confirmed live):

```r
fit <- linear_reg() |> set_engine("lm") |> fit(mpg ~ ., data = mtcars)
predict_raw(fit, mtcars, opts = list(newdata = mtcars))
#> Error in opts[[!dup_args]] :
#>   attempt to select less than one element in integerOneIndex
```

Note that the colliding name must actually match a protected argument for that engine's raw prediction module (for lm these are `object` and `newdata`, per `object$spec$method$pred$raw$args`); a non-colliding name like `type` never reaches the buggy branch.

## Root cause

R/predict_raw.R:6-10 in `predict_raw.model_fit()`:

```r
protected_args <- names(object$spec$method$pred$raw$args)
dup_args <- names(opts) %in% protected_args
if (any(dup_args)) {
  opts <- opts[[!dup_args]]
}
```

`dup_args` is a logical vector, and the intent (per the surrounding code, which afterwards appends the surviving `opts` to the prediction args) is to keep the non-colliding entries. But line 9 uses `[[`, which requires a single scalar index. With one colliding entry, `!dup_args` is `FALSE`, which `[[` coerces to integer `0`, producing "attempt to select less than one element". With multiple entries it fails with the "subscript out of bounds"/length errors instead. Every code path through this branch errors; the drop never happens.

## Proposed fix

In R/predict_raw.R, change the subsetting to single-bracket and warn about the dropped entries:

```r
protected_args <- names(object$spec$method$pred$raw$args)
dup_args <- names(opts) %in% protected_args
if (any(dup_args)) {
  cli::cli_warn(
    "{cli::qty(sum(dup_args))}The argument{?s} {.arg {names(opts)[dup_args]}}
     in {.arg opts} {?is/are} protected and will be ignored."
  )
  opts <- opts[!dup_args]
}
```

Warn versus silent drop: the issue's minimal suggestion is `opts <- opts[!dup_args]` (silent drop, matching the original intent of the code). A warning is recommended instead because a user who passes `newdata` or `object` through `opts` believes those values will be used — silently ignoring them can yield predictions on different data than the user supplied, with no signal. `predict.model_fit()` already sets this precedent by warning when `opts` is ignored for non-raw types (R/predict.R:180-184). The warning costs nothing on the happy path since it sits inside `if (any(dup_args))`. Recommendation: warn.

## Tests

The changed function lives in R/predict_raw.R, so tests go in tests/testthat/test-predict_raw.R (new file; no test file currently exists for R/predict_raw.R, and no existing tests call `predict_raw()` directly).

- `expect_snapshot()` around `predict_raw(fit, mtcars, opts = list(newdata = mtcars))` to capture the new warning, then check the returned value equals `predict_raw(fit, mtcars)` (the colliding option must not change the result).

- A non-colliding `opts` entry still passes through: e.g. `predict_raw(fit, mtcars, opts = list(type = "terms"))` returns the terms matrix without warning (`expect_no_condition()` plus a class/dimension check on the result).

- Both tests use an lm fit, so no extra Suggests packages are needed.

## Follow-ups / risks

Draft NEWS.md bullet:

- `predict_raw()` no longer errors when `opts` contains an argument name that collides with a protected prediction argument (such as `newdata` or `object`); the colliding entry is now dropped with a warning (#1408).

Risks and related items:

- Behavior change: code that previously errored now warns and predicts; no working code changes behavior, since every path through the buggy branch errored before.

- If maintainers prefer the strictly minimal fix (silent drop, exactly as suggested in the issue body), remove the `cli_warn()` call; the test then becomes an `expect_no_condition()` plus a result-equality check.

- `predict.model_fit(type = "raw")` funnels through this function (R/predict.R:205), so the fix also covers `predict(..., type = "raw", opts = ...)`; worth one test via that route as well.

## Work items

Executed 2026-09-22 on branch `model-predict-args` (off `main` at 67ace4e1), together with [issue 1410](2026-09-09-issue-1410-multi-predict-args-workflow.md).

- [x] Change `opts[[!dup_args]]` to `opts[!dup_args]` in `R/predict_raw.R` and warn about the dropped entries
- [x] Add `tests/testthat/test-predict_raw.R` (new file)
- [x] Cover the `predict(type = "raw")` route as well
- [x] Add the `NEWS.md` bullet
- [x] `air format .` and full `R CMD check`

### Notes from execution

Took the recommended warn-rather-than-silently-drop option, matching the precedent at `R/predict.R:179-183`.

`cli::qty()` handles the singular/plural split, verified both ways:

```
The argument `newdata` in `opts` is protected and will be ignored.
The arguments `newdata` and `object` in `opts` are protected and will be ignored.
```

The tests assert the returned value equals `predict_raw(fit, mtcars)` in all three colliding cases, so the drop is confirmed to be a no-op on the result rather than merely non-erroring. The unprotected case checks `opts = list(type = "terms")` reaches the engine by asserting the returned matrix's dimensions and column names, which is stronger than the class check the plan suggested.

No new Suggests: both tests use an `lm` fit on `mtcars`.
