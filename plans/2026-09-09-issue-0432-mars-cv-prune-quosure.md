# Issue #432: mars fit fails if prune_method='cv' and prod_degree is specified

## Issue

- Number: #432
- Title: mars fit fails if prune_method='cv' and prod_degree is specified
- URL: https://github.com/tidymodels/parsnip/issues/432
- Filed: 2021-02-16
- Current labels: `bug`
- Classification: the `bug` label is correct. `translate()` shows a valid `earth::earth()` call and the equivalent direct earth call succeeds, so this is a parsnip defect in how the model call is constructed. A second reporter (dchiu911) notes the practical consequence: `prune_method = tune()` is unusable whenever `"cv"` is in the grid, because the fix for the "nfold must be specified" error (setting `nfold` as an engine argument) immediately trips this quosure error for the cv candidates.

## Symptom

Reproduced live against the current checkout (earth 5.x installed):

```r
devtools::load_all("/Users/max/github/parsnip")
set.seed(28193)
n_obs <- 200
dat <- data.frame(x1 = rnorm(n_obs, 5, 3), x2 = rnorm(n_obs, 2, 1))
dat$y <- dat$x1 + dat$x2 + rnorm(n_obs, sd = 0.5)

mod <- mars(mode = "regression", prod_degree = 2, prune_method = "cv") |>
  set_engine("earth", nfold = 5, ncross = 2)
fit(mod, y ~ ., data = dat)
#> Error : 'degree' must be numeric, or TRUE or FALSE (whereas its current
#> class is "quosure,formula")
```

The identical spec with `prune_method = "backward"` (or `"none"`, or with
`prod_degree` unset) fits fine. Also verified live: `num_terms = 4` and the
engine argument `nk = 10` do *not* trigger the error with `"cv"`; only
`prod_degree`/`degree` does (see root cause for why).

The stored engine call of a successful fit shows the underlying problem —
quosures are embedded literally in the call:

```r
mod2 <- mars(mode = "regression", prod_degree = 2, prune_method = "backward") |>
  set_engine("earth")
fit(mod2, y ~ ., data = dat)$fit$call
#> earth(formula = y ~ ., data = data, pmethod = ~"backward", keepxy = TRUE,
#>     degree = ~2)
```

## Root cause

Verified against current source and with a live trace on
`earth:::check.integer.scalar()`:

- `mars()` stores its arguments as quosures via `enquo()`
  (/Users/max/github/parsnip/R/mars.R:41-45).
- `translate.default()` copies those quosures into `x$method$fit$args`
  (/Users/max/github/parsnip/R/translate.R:106).
- `make_form_call()` splices them, still as quosure objects, into the
  constructed call via `make_call()`
  (/Users/max/github/parsnip/R/arguments.R:219-272 and 198-216). The fit call
  is therefore `earth::earth(..., degree = <quosure>, pmethod = <quosure>, ...)`.
- `eval_mod()` evaluates that call with `rlang::eval_tidy()`
  (/Users/max/github/parsnip/R/fit.R:368-385). On this first pass the embedded
  quosures evaluate correctly when earth forces its argument promises, which is
  why every non-cv pruning path works (juliasilge's diagnosis in the thread).
- earth, however, records the *unevaluated* call (with `degree = ~2`) in the
  fitted object. With `pmethod = "cv"`, earth re-dispatches internally:
  `earth.formula()` calls `update.earth()`, which rebuilds the stored call and
  re-evaluates it with base `eval.parent()` — no tidy evaluation, so the
  quosure literal survives and `earth:::check.integer.scalar(degree, ...)`
  errors. Live-captured stack at the failure:

```text
fit -> fit.model_spec -> form_form -> eval_mod -> eval_tidy -> earth::earth
  -> earth.formula -> update.earth -> eval.parent -> eval -> eval
  -> earth -> earth.formula -> earth.fit -> check.integer.scalar
```

- Only `degree` is exposed because earth's cv machinery explicitly overrides
  the other quosure-carrying arguments in its re-invocations (`pmethod` is
  forced to `"backward"`, `nfold = 0`, `ncross = 0`, and `nprune` is set per
  candidate), while `degree` is carried through from the stored call.

parsnip already works around this exact disease in two places, which confirms
the diagnosis and provides the pattern to reuse:

- `multi_predict._earth()` evaluates quosures left in `object$fit$call`
  before calling `update()` (/Users/max/github/parsnip/R/mars.R:159-167).
- `repair_call()` does the same cleanup on request
  (/Users/max/github/parsnip/R/repair_call.R:43-50).

Issue #1069 (glmnet `relax = TRUE` fails) is the same defect: glmnet's relax
path re-evaluates the stored call and hits the raw quosures.

## Proposed fix

Evaluate quosures when the fit call is constructed, so engines that
re-evaluate their stored call never see them.

Primary approach: in `make_form_call()` and `make_xy_call()`
(/Users/max/github/parsnip/R/arguments.R:219-315), pass `fit_args` through the
existing `maybe_eval()` helper (/Users/max/github/parsnip/R/arguments.R:151-158)
before `make_call()`:

```r
fit_args <- purrr::map(fit_args, maybe_eval)
```

- `maybe_eval()` falls back to the quosure when evaluation fails, so
  descriptor arguments (`.cols()` etc.) are safe: `scoped_descrs()` has already
  run by the time `make_form_call()` is called in `form_form()`
  (/Users/max/github/parsnip/R/fit_helpers.R:38-49).
- This happens at fit time, after tune has finalized any `tune()`
  placeholders, so tuning is unaffected.
- This also fixes #1069 and any other engine that re-evaluates its stored
  call with base `eval()`.

Verified live that evaluated arguments fix the failure (simulating the fix
with the existing `eval_args()` helper, /Users/max/github/parsnip/R/arguments.R:165-169):

```r
mod_fixed <- eval_args(mod)
fit(mod_fixed, y ~ ., data = dat)$fit$call
#> earth(formula = y ~ ., data = data, pmethod = "cv", keepxy = TRUE,
#>     degree = 2, nfold = 3)
```

Alternative (scoped, if the general change is judged too invasive): call
`eval_args(x)` inside `translate.mars()` (/Users/max/github/parsnip/R/mars.R:93-111),
mirroring the glmnet helpers that call `eval_args(object$spec)`
(/Users/max/github/parsnip/R/glmnet-engines.R:48-68, 206). This fixes only the
earth engine and leaves #1069 open. No maintainer disagreement is recorded in
the thread; juliasilge only diagnosed the mechanism.

## Tests

Add to /Users/max/github/parsnip/tests/testthat/test-mars.R (next to the
existing `'classification'` and `'mars execution'` tests):

- `test_that("prune_method = 'cv' works with prod_degree", ...)` with
  `skip_if_not_installed("earth")`: fit
  `mars(mode = "regression", prod_degree = 2, prune_method = "cv") |> set_engine("earth", nfold = 3)`
  on a small simulated data set, then
  `expect_s3_class(extract_fit_engine(f), "earth")`.
- Assert the constructed call carries no quosures:
  `expect_all_false(purrr::map_lgl(as.list(extract_fit_engine(f)$call), rlang::is_quosure))`.
- If the general `make_form_call()`/`make_xy_call()` fix is taken, run the full
  suite: existing snapshots that print model-fit templates or stored calls
  (`translate()` output is unaffected, but `$fit$call` printing changes from
  `degree = ~2` to `degree = 2`) may need re-review.

No new snapshot tests are needed for the fix itself since the fixed path
succeeds; the existing behavior for unevaluable arguments is preserved by
`maybe_eval()`.

## Follow-ups / risks

- Draft NEWS.md bullet: `mars()` fits with the earth engine no longer fail when `prune_method = "cv"` is combined with `prod_degree`; parsnip now evaluates quosure arguments when building the model call (#432).
- Behavior change: `object$fit$call` now stores evaluated values instead of quosure literals. This is strictly an improvement (it is what `repair_call()` produces), but extension packages or user code that inspected the raw quosures could notice; check reverse dependencies' snapshots.
- The quosure-evaluation loop in `multi_predict._earth()` (/Users/max/github/parsnip/R/mars.R:159-167) becomes dead code after the general fix and can be simplified in a follow-up.
- Related: #1069 (glmnet `relax = TRUE`) is the same defect and is fixed by the general approach; note that in its thread when closing.
- Tunability follow-up from the thread: after this fix, `prune_method = tune()` with `"cv"` in the grid still requires users to set `nfold` as an engine argument (earth errors otherwise); that is earth behavior, not a parsnip bug, but worth a documentation note in the earth engine docs.
- Not addressed here: `prune_method = "exhaustive"` failures reported in the issue body reproduce in earth directly and are out of scope.

## Work items

Executed 2026-09-22 on branch `quosure-eval-bugs` (off `main` at aff7f0f1), together with [issue 1069](2026-09-09-issue-1069-glmnet-relax.md).

- [x] Reproduce both failures
- [x] Take the general fix: evaluate quosure fit args in `make_form_call()` and `make_xy_call()`
- [x] Measure the blast radius across the full suite before committing to the approach
- [x] Update the four ranger descriptor assertions
- [x] Tests for #432 in `test-mars.R` and #1069 in a new `test-glmnet-engines.R`
- [x] Shared `NEWS.md` bullet
- [x] `air format .` and full `R CMD check`

### The two plans disagreed; the general fix won on measured evidence

This plan recommended the general fix; the #1069 plan explicitly rejected it as too large a blast radius and wanted a glmnet-targeted fix, deferring the general version into #878. Rather than pick on intuition, the general fix was applied and the full suite run.

Measured fallout: **four assertions in one test**, all the same thing, and no snapshot churn anywhere.

```
test-rand_forest_ranger.R:362, 376, 390, 404 - "additional descriptor tests"
  actual:   double vector (7, 20, 10, 1)
  expected: <quosure/formula>
```

That is small enough that two separate fixes were not justified, especially as the general version also inoculates every other engine that re-evaluates its recorded call.

### Where this plan's reasoning was wrong

This plan predicted descriptors would be unaffected because `maybe_eval()` falls back when evaluation fails. That is incorrect: `scoped_descrs()` has **already run** by the time the call is assembled, so `min(.lvls())` evaluates *successfully* to `7`. Descriptors do not fall back, they resolve.

So the general fix does change one deliberately tested behaviour — ranger's recorded call now holds `class.weights = c(7, 20, 10, 1)` instead of the quosure. Verified the fit itself is identical: same `mtry`, same `num.trees`, same `prediction.error`, same predictions. Only the recorded call differs, and the new content is what `repair_call()` would have produced anyway.

topepo confirmed that descriptors are being removed or disabled and that breaking them is acceptable, which settled the question. The four assertions were updated to expect `c(min(table(hpc$class)), 20, 10, 1)` — the resolved value, written so it is not a magic number — and the now-meaningless `ignore_formula_env = TRUE` arguments were dropped.

### Implementation note

`make_xy_call()` computed a local `fit_args` and then never used it; it mutates and passes `object$method$fit$args` to `make_call()` instead. That local was dead code before this change. The fix therefore assigns back into `object$method$fit$args` on the xy path and into the local on the formula path, with a comment noting the asymmetry.

The helper gates on `rlang::is_quosure()` so data placeholders (`sym()`, `expr(missing_arg())`) are untouched, and reuses `maybe_eval()` so genuinely unevaluable expressions still fall back rather than erroring at fit time.

### Follow-up now unblocked

The quosure-evaluation loop in `multi_predict._earth()` (`R/mars.R:159-167`) and the equivalent in `repair_call()` are now dead for these paths and can be simplified once descriptors are gone.
