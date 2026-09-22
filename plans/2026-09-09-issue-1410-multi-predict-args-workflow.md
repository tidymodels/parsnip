# Issue #1410: `multi_predict_args.workflow()` returns the wrong object instead of argument names

## Issue

- Number: #1410

- Title: `multi_predict_args.workflow()` returns a `model_fit`/workflow object instead of argument names

- URL: https://github.com/tidymodels/parsnip/issues/1410

- Filed: 2026-09-01 by EmilHvitfeldt (maintainer); no comments

- Labels: `tidy-dev-day :nerd_face:` only. This is a bug (method never returns the documented character vector, and the internal workflow path it reads from is stale); a `bug` label is missing but the classification in the issue body is accurate. The issue title is slightly out of date: on current workflows the method returns bare `NULL`, not a `model_fit` object, because `object$fit$model$model` no longer exists — the issue body itself notes this.

## Symptom

Repro (run against current main with `devtools::load_all()`; kknn and workflows installed; output confirmed live):

```r
library(workflows)
wf <- workflow() |>
  add_formula(mpg ~ .) |>
  add_model(nearest_neighbor(neighbors = 7) |> set_engine("kknn") |> set_mode("regression"))
wf_fit <- fit(wf, data = mtcars)

multi_predict_args(wf_fit)
#> NULL
has_multi_predict(wf_fit)
#> [1] FALSE
names(wf_fit$fit)
#> [1] "actions" "fit"
multi_predict_args(wf_fit$fit$fit)
#> [1] "neighbors"
```

The workflow methods return `NULL`/`FALSE` even though the underlying parsnip fit correctly reports `"neighbors"`.

## Root cause

Two defects in R/aaa_multi_predict.R, both from the same stale internal path.

- R/aaa_multi_predict.R:136-138, `multi_predict_args.workflow()`:

```r
multi_predict_args.workflow <- function(object, ...) {
  object <- object$fit$model$model
}
```

The body is a bare assignment: it never calls `multi_predict_args()` on the extracted object, so even when the path existed it returned the fitted model (invisibly) instead of the documented character vector of submodel argument names. On current workflows, the fitted parsnip model lives at `object$fit$fit` (confirmed live: `names(wf_fit$fit)` is `"actions", "fit"`), so `object$fit$model$model` is `NULL` and the method returns invisible `NULL`.

- R/aaa_multi_predict.R:95-97, `has_multi_predict.workflow()`, has the same stale-path defect:

```r
has_multi_predict.workflow <- function(object, ...) {
  has_multi_predict(object$fit$model$model)
}
```

`object$fit$model$model` is `NULL`, so this dispatches to `has_multi_predict.default()` and always returns `FALSE`, even for models with submodel prediction (confirmed live above).

## Proposed fix

Fix both methods in R/aaa_multi_predict.R by delegating through the supported extractor instead of hardcoding workflows internals (hardcoded internals are exactly what rotted here; parsnip already uses `hardhat::extract_fit_parsnip()` in R/misc.R:684, and hardhat is in Imports):

```r
has_multi_predict.workflow <- function(object, ...) {
  has_multi_predict(hardhat::extract_fit_parsnip(object), ...)
}

multi_predict_args.workflow <- function(object, ...) {
  multi_predict_args(hardhat::extract_fit_parsnip(object), ...)
}
```

The issue body suggests the more minimal `multi_predict_args(object$fit$fit, ...)`; that also works today but re-introduces a hardcoded workflows internal path. Recommendation: use `hardhat::extract_fit_parsnip()`. Dispatch is not a concern — the workflows package must be loaded for a `workflow` object to exist, and it registers the `extract_fit_parsnip.workflow` method.

## Tests

The changed functions live in R/aaa_multi_predict.R, so tests go in tests/testthat/test-aaa_multi_predict.R (new file). Existing `has_multi_predict()`/`multi_predict_args()` tests for parsnip objects live in tests/testthat/test-misc.R; the new workflow tests can reference those for style, but per project convention new tests for this source file belong in the matching test file.

- Fit a workflow with `nearest_neighbor(neighbors = 7)` + kknn (kknn is already in Suggests) and assert `expect_equal(multi_predict_args(wf_fit), "neighbors")` and `expect_true(has_multi_predict(wf_fit))` — or, per project conventions, a specific expectation such as `expect_identical()` for the character vector.

- A negative case: a workflow wrapping `linear_reg()` + lm returns `NA_character_` from `multi_predict_args()` and `FALSE` from `has_multi_predict()`.

- Guard with `skip_if_not_installed("workflows")` and `skip_if_not_installed("kknn")`.

Dependency note: workflows is not currently in parsnip's DESCRIPTION Suggests, so using it in tests requires adding it (a workflows -> parsnip Imports edge already exists; the reverse Suggests edge is allowed and common in tidymodels). If maintainers do not want the new Suggests entry, the fallback is testing the delegation with the `object$fit$fit` variant against a manually constructed `structure(list(fit = list(fit = knn_fit)), class = "workflow")`, but that couples the test (and fix) to workflows internals again — not recommended.

## Follow-ups / risks

Draft NEWS.md bullet:

- `multi_predict_args()` and `has_multi_predict()` now work again for fitted workflows, returning the submodel argument names and `TRUE` respectively; they previously returned `NULL` and `FALSE` because they read from an outdated workflows internal structure (#1410).

Risks and related items:

- Behavior change for untrained workflows: previously `has_multi_predict()` returned `FALSE` and `multi_predict_args()` returned invisible `NULL`; with the fix, `hardhat::extract_fit_parsnip()` errors on an untrained workflow ("Can't extract a model fit from an untrained workflow"). That error is arguably more informative than a silent wrong answer, but it is a change; if it matters, wrap in a trained-workflow check or fall back to the spec.

- `multi_predict_args.workflow()` previously returned its value invisibly (assignment as last expression); the fix makes the return visible, matching the other methods.

- Check tune and workflows revdeps for callers of these generics on workflow objects; tune calls `multi_predict_args()` on parsnip fits (not workflows) in its submodel handling, so impact should be nil, but a quick grep of tune/finetune is cheap.

- Blocker: none; the fix is self-contained. The only decision needed is the Suggests addition for tests.

## Work items

Executed 2026-09-22 on branch `model-predict-args` (off `main` at 67ace4e1), together with [issue 1408](2026-09-09-issue-1408-predict-raw-opts.md).

- [x] Fix `has_multi_predict.workflow()` to delegate through `hardhat::extract_fit_parsnip()`
- [x] Fix `multi_predict_args.workflow()` the same way, and actually return the value
- [x] Confirm the fix changes behavior (tests fail without it, pass with it)
- [x] Add the `NEWS.md` bullet
- [x] `air format .` and full `R CMD check`
- [x] Hand the cross-package tests over for extratests rather than adding `workflows` to Suggests

### The Suggests question, resolved

Do **not** add `workflows` to parsnip's `Suggests`. parsnip sits low in the tidymodels dependency graph and workflows Imports it, so the reverse edge risks an indirect circular dependency. Cross-package tests belong in the separate `extratests` repo.

The tests were written in parsnip temporarily, only to confirm the fix, then removed. Verified they discriminate — the same file against unfixed source:

```
                             without fix      with fix
multi_predict_args(knn wf)   NULL             "neighbors"
has_multi_predict(knn wf)    FALSE            TRUE
multi_predict_args(lm wf)    NULL             NA_character_
has_multi_predict(lm wf)     FALSE            FALSE
```

The test file and its snapshot are handed to topepo for a PR against extratests. Their content is reproduced below so it is not lost with the branch.

### Untrained workflows

The behavior change the plan flagged is real and is now covered by a snapshot in the handed-over tests. Previously `multi_predict_args()` returned invisible `NULL` and `has_multi_predict()` returned `FALSE` for an untrained workflow; both now error through hardhat:

```
Error in `hardhat::extract_fit_parsnip()`:
! Can't extract a model fit from an untrained workflow.
i Do you need to call `fit()`?
```

That is a better answer than silently claiming the workflow has no submodel arguments, so it was kept rather than guarded against.

### Test code for extratests

```r
test_that('multi_predict helpers work on fitted workflows', {
  skip_if_not_installed("workflows")
  skip_if_not_installed("kknn")
  # Issue 1410

  library(workflows)

  knn_fit <-
    workflow() |>
    add_formula(mpg ~ .) |>
    add_model(
      nearest_neighbor(neighbors = 7) |>
        set_engine("kknn") |>
        set_mode("regression")
    ) |>
    fit(data = mtcars)

  expect_identical(multi_predict_args(knn_fit), "neighbors")
  expect_identical(has_multi_predict(knn_fit), TRUE)

  # the workflow and the parsnip fit it wraps agree
  expect_identical(
    multi_predict_args(knn_fit),
    multi_predict_args(extract_fit_parsnip(knn_fit))
  )

  lm_fit <-
    workflow() |>
    add_formula(mpg ~ .) |>
    add_model(linear_reg() |> set_engine("lm")) |>
    fit(data = mtcars)

  expect_identical(multi_predict_args(lm_fit), NA_character_)
  expect_identical(has_multi_predict(lm_fit), FALSE)
})

test_that('multi_predict helpers error on untrained workflows', {
  skip_if_not_installed("workflows")
  # Issue 1410

  library(workflows)

  wf <-
    workflow() |>
    add_formula(mpg ~ .) |>
    add_model(linear_reg() |> set_engine("lm"))

  expect_snapshot(error = TRUE, multi_predict_args(wf))
  expect_snapshot(error = TRUE, has_multi_predict(wf))
})
```
