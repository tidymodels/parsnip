# Remove `$` partial matching from the fit and predict paths

## Overview

Two places in parsnip reached into a list with `$` using a name that does not exist, relying on R's partial matching to find a longer one. Both worked, and both were silent time bombs: adding an element whose name is a prefix of the intended one would have redirected the access without any error.

Found by sweeping the test suite under `options(warnPartialMatchDollar = TRUE)`. That sweep was prompted by the same defect in `R/bart.R` (`obj$lv` for `obj$lvl`), fixed earlier in #1407.

No issue number; this came out of the sweep directly. No `NEWS.md` bullet — results are unchanged and the project conventions exclude internal fixes.

## Work items

- [x] Sweep the full suite under `warnPartialMatchDollar = TRUE`
- [x] Fix `R/fit_helpers.R:52`
- [x] Fix `R/rand_forest_data.R:352`
- [x] Fix the mirroring accesses in `tests/testthat/test-rand_forest_ranger.R`
- [x] Add guard tests, confirmed to fail against unfixed source
- [x] Re-run the sweep, which exposed a third site hidden behind the first
- [x] Fix `tests/testthat/test-mlp.R:27`
- [x] Fix `R/aaa_multi_predict.R:19`, found via a stray snapshot rather than the sweep
- [x] Final sweep: parsnip's own code is clean
- [x] `air format .` and full `R CMD check`

## The defects

**`R/fit_helpers.R:52`, the significant one.** `levels_from_formula()` (`R/misc.R:292-301`) returns a list with `lvls` and `ordered`, but `form_form()` read `y_levels$lvl`:

```
levels_from_formula() returns: lvls ordered
  $lvls : setosa versicolor virginica
  $lvl  : setosa versicolor virginica   <- resolved only by partial matching
```

This sits on the formula fit path for every model, and the value becomes `model_fit$lvl` — the outcome factor levels consumed throughout prediction. Had an `lvl` element ever been added to that list, every classification fit would have silently taken the wrong levels.

**`R/rand_forest_data.R:352`.** A prediction `post` function did `x <- x$prediction`; ranger returns `predictions`.

Verified both are behaviour-neutral: `model_fit$lvl` is unchanged for a `glm` classification fit, and ranger predictions are unchanged.

**`tests/testthat/test-mlp.R:27`, found on the verification pass.** The first sweep hid this one: every classification fit was already emitting `lvl`/`lvls` warnings from `form_form()`, so this site only became visible once that was fixed. Here the package code is right and the *test* was wrong — `nnet_softmax()` reads `object$lvl`, which is what a fitted model carries, but the test set `obj$lvls` on a spec and passed only through partial matching. It was exercising a shape that never occurs in production. Corrected to `obj$lvl`.

**`R/aaa_multi_predict.R:19`, found by a different route.** The `multi_predict()` generic tested `inherits(object$fit, "try-error")` *before* dispatch, so `object` can be anything. An `earth` object has `fitted.values`, so `$fit` partial matched to it.

This one never showed up in the sweep counts, and the reason is worth recording: the call that triggers it sits inside `expect_snapshot()`, so the warning was captured into the snapshot instead of being emitted as a test warning. Grepping the reporter output could not see it. It surfaced only as an unexplained `_snaps/misc.new.md` left behind by a sweep run. Any future sweep should check for stray `.new.md` files as well as reading the warning counts.

Fixed by switching to `object[["fit"]]`, which is exact by default. Behaviour is unchanged: the earth object still produces `No multi_predict() method exists for objects with classes <earth>`, and the `model_fit` path is unaffected.

## Ruled out

- `partial match of 'fit' to 'fitted.values'` traces into `kknn::predict.kknn()`, confirmed by walking the call stack and counting frames: of the warnings raised on that path, all come from `predict.kknn` and none from parsnip. Upstream, not parsnip's to fix. These 29 hits are all that the final sweep reports.
- Six `$prediction` accesses in `test-rand_forest_ranger.R` were test-only and cosmetic, but they mirrored the package bug and were corrected alongside it.

## Guard tests

Both use `withr::local_options(warnPartialMatchDollar = TRUE)` with `expect_no_condition()`, matching the pattern added for BART in #1407. Confirmed they fail against unfixed source:

```
FAILURE: 'test-fit_interfaces.R:206:3'
FAILURE: 'test-fit_interfaces.R:209:3'
FAILURE: 'test-rand_forest_ranger.R:589:3'
```

## Merge notes

Branch `no-partial-matches`, off `main` at 15deaf38. Two branches were open at the same time:

- `glmnet-predict-fixes` (#857, #878) — no files in common.
- `argument-passing-processing` (#492, #1251) — also touches `R/fit_helpers.R`, but its nearest hunk starts at line 59 while this change is at line 52. Seven lines apart with three lines of diff context, so the hunks do not touch and either merge order is clean.

Skipping the `NEWS.md` bullet also avoids the only file all three branches would otherwise have contended over.
