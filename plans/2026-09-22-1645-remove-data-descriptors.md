# Issue #1433: Remove data descriptors

## Overview

Remove parsnip's data descriptors — `.cols()`, `.preds()`, `.obs()`, `.lvls()`, `.facts()`, `.x()`, `.y()`, `.dat()` — and the machinery that supports them.

- Issue: [#1433](https://github.com/tidymodels/parsnip/issues/1433), filed 2026-09-22
- Position in the queue: **last**. Nothing else depends on it.

Descriptors are the main reason model and engine arguments are held as quosures all the way to the fit call. That laziness is what leaked raw quosures into the call an engine records, which is the shared root cause of [#432](2026-09-09-issue-0432-mars-cv-prune-quosure.md) and [#1069](2026-09-09-issue-1069-glmnet-relax.md).

**Both of those are already fixed** on branch `quosure-eval-bugs`, by evaluating quosure arguments while the fit call is assembled. So this issue is not blocking them. What remains is the simplification that removal unlocks.

## Status

- [x] File the issue
- [ ] topepo: run reverse-dependency checks against a branch with descriptors disabled
- [ ] Decide deprecate-then-defunct versus outright removal, based on those results
- [ ] Remove `descr_env` and the `poke_descrs()`/`scoped_descrs()` machinery
- [ ] Drop the two `requires_descrs()` branches in `R/fit_helpers.R`
- [ ] Simplify `maybe_eval()` to a plain `eval_tidy()`
- [ ] Drop the redundant quosure cleanups in `multi_predict._earth()` and `repair_call()`
- [ ] Update `man/descriptors.Rd` and the references in `R/model_object_docs.R`
- [ ] Update the four "additional descriptor tests" assertions in `test-rand_forest_ranger.R`

## Footprint

Well isolated:

| | |
|---|---|
| `R/descriptors.R` | 432 lines |
| `tests/testthat/test-descriptors.R` | 237 lines |
| Exported functions | 8 |
| Call sites outside `R/descriptors.R` | 2, both in `R/fit_helpers.R` (lines 38-40, 101-103) |
| Documentation | `man/descriptors.Rd`, `R/model_object_docs.R:117-121` and `:216` |

The only other test in the suite that depends on descriptor behaviour is `test-rand_forest_ranger.R`'s "additional descriptor tests", which fits ranger with `class.weights = c(min(.lvls()), 20, 10, 1)`. Those four assertions were already updated on `quosure-eval-bugs` to expect the resolved value rather than the quosure, so they will need only a further trim.

## Design notes

`maybe_eval()` (`R/arguments.R:150-158`) wraps evaluation in `try()` and silently falls back to the unevaluated quosure, with the comment "if descriptors are in `x`, eval fails". This matters for sequencing: while that fallback is in place, a descriptor that errors is **silently** replaced by the quosure and handed to the engine, producing a confusing downstream failure instead of the intended message. Any disable or deprecation step has to address the fallback at the same time, or the clear error never reaches the user. This also makes the fallback the right thing to remove first once descriptors are gone.

`.x()`, `.y()` and `.dat()` return the training data itself rather than a summary statistic, so they are a different kind of thing from `.cols()` and `.obs()`. Worth deciding separately whether any of them should survive in another form.

## Context

topepo confirmed on 2026-09-22 that descriptors are to be removed or disabled and that breaking them is acceptable, provided failures are clear warnings or errors rather than silent wrong answers. That decision is what allowed the general quosure fix on `quosure-eval-bugs` to be taken over two narrower engine-specific fixes.
