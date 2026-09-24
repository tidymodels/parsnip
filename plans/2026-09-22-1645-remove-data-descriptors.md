# Issue #1433: Remove data descriptors

## Overview

Remove parsnip's data descriptors — `.cols()`, `.preds()`, `.obs()`, `.lvls()`, `.facts()`, `.x()`, `.y()`, `.dat()` — and the machinery that supports them.

- Issue: [#1433](https://github.com/tidymodels/parsnip/issues/1433), filed 2026-09-22
- Position in the queue: **last**. Nothing else depends on it.

Descriptors are the main reason model and engine arguments are held as quosures all the way to the fit call. That laziness is what leaked raw quosures into the call an engine records, which is the shared root cause of [#432](2026-09-09-issue-0432-mars-cv-prune-quosure.md) and [#1069](2026-09-09-issue-1069-glmnet-relax.md).

**Both of those are already fixed** on branch `quosure-eval-bugs`, by evaluating quosure arguments while the fit call is assembled. So this issue is not blocking them. What remains is the simplification that removal unlocks.

## Status

Executed 2026-09-23 on branch `no-data-descriptors` (off `main` at e9e464da).

topepo chose outright removal over deprecate-then-defunct, with the reverse-dependency check run **against the removal itself** rather than a disable branch — a stronger signal, since anything that uses a descriptor fails outright.

- [x] File the issue
- [x] Delete `R/descriptors.R` (432 lines), including `descr_env` and the `poke_descrs()`/`scoped_descrs()` machinery
- [x] Drop the two `requires_descrs()` branches in `R/fit_helpers.R`
- [x] Delete `tests/testthat/test-descriptors.R` and its snapshots
- [x] Remove the `additional descriptor tests` block from `test-rand_forest_ranger.R`
- [x] Rewrite the descriptor prose in `R/model_object_docs.R` and re-document, which drops `man/descriptors.Rd`
- [x] Remove the `descriptors` entry from `_pkgdown.yml`
- [x] `NEWS.md` bullet under Breaking Change
- [x] Drop `globals` from `DESCRIPTION` Imports
- [x] `air format .` and full `R CMD check`
- [x] `revdepcheck::cloud_check()` and report
- [ ] Follow-up, deliberately deferred: simplify `maybe_eval()`, and the quosure cleanups in `multi_predict._earth()` and `repair_call()`

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


## Execution notes

### What came out

All 8 exports (`.cols`, `.preds`, `.obs`, `.lvls`, `.facts`, `.x`, `.y`, `.dat`) dropped from `NAMESPACE`, `man/descriptors.Rd` deleted, and the two `requires_descrs()` blocks in `R/fit_helpers.R` removed. Those two were the only real call sites outside `R/descriptors.R`; everything else that matched a `descr` grep was unrelated roxygen prose.

Two documentation spots advertised descriptors and were rewritten rather than deleted: the `model_spec` help suggested `rand_forest(mtry = .cols() - 1)` as the *recommended* way to avoid referencing globals, and a `model_fit` example used `set_engine("lm", x = .obs() < 500)`. The latter became `x = TRUE`, which demonstrates the same point about keeping the `x` matrix without needing a descriptor.

### `maybe_eval()` deliberately left alone

`R/arguments.R`'s `maybe_eval()` wraps evaluation in `try()` with the comment "if descriptors are in `x`, eval fails". The issue listed collapsing it to a bare `eval_tidy()` as step 4. That was **not** done here, on purpose: an argument can still refer to something unavailable at that point, so removing the fallback would convert silent fallbacks into hard errors — a behaviour change with its own blast radius, unrelated to descriptors. Folding it in would also muddy the reverse-dependency signal, which is the whole point of this branch. The comment was corrected to say why the `try()` remains.

### A dependency fell out

The first check produced `Namespace in Imports field not imported from: 'globals'`. `R/descriptors.R` was the package's only consumer of globals — `has_any_descrs()` used `globals::globalsOf()` to detect descriptor calls inside a quosure. With the file gone the Import is dead, so it was dropped from `DESCRIPTION`. Verified no other `globals::` usage remains anywhere in `R/` or `tests/`.

### Test suite

Removing descriptors causes **no** test failures.

A full `devtools::test()` on this branch reports one failure, `test-sparsevctrs.R:383`. It is pre-existing and unrelated: the identical failure occurs on `main` at e9e464da, both in a full run and with a two-file filter of `^(glmnet-engines|sparsevctrs)`. Isolated, `test-sparsevctrs.R` passes 30/30.

The cause is test pollution. `test-sparsevctrs.R` mocks `glmnet::predict.elnet` with `local_mocked_bindings(.package = "glmnet")`; once an earlier file has already dispatched a glmnet prediction, the mock no longer takes effect and the expected error never fires. `test-glmnet-engines.R`, added for #1069 and #857, is that earlier file. It does not affect `R CMD check`, which runs tests against the installed package. This is exactly the hazard the project conventions warn about for the `.package` argument, and it needs its own fix rather than being folded in here.

`pkgdown::check_pkgdown()` cannot run in this environment — it aborts in `find_template_config()` on both this branch and the base, so the failure is environmental. The `_pkgdown.yml` edit is self-consistent: the `descriptors` entry is gone and so is the topic it pointed at.


## Reverse-dependency results

`revdepcheck::cloud_check()`, job `17824d2c-b62d-43a2-af9c-5cffdd682179`, finished `SUCCEEDED [0/0/103/0 - 103]` in 37 minutes across 103 reverse dependencies (102 CRAN + 1 Bioconductor).

**The hypothesis holds: not one reverse dependency uses a data descriptor.** Removing all eight functions outright — not merely disabling them — produced zero descriptor-related failures. Had any package called `.cols()`, `.lvls()` or a sibling, it would have failed hard with an object-not-found error. None did.

### One new problem, and it is not this branch

`tidypredict` fails to rebuild its `lm.Rmd` vignette. The cause is **#492**, which is already on `main`, not the descriptor removal:

```
Quitting from lm.Rmd:123-131
Error in `fit()`:
! `...` must be empty.
x Problematic argument: `offset`.
    fit(., mpg ~ wt + cyl, offset = am, data = mtcars)
```

The revdep compares CRAN parsnip against this branch, and this branch is based on `main`, so every merged change is in scope — which is why a #492 regression surfaced in a descriptor run.

This is the risk flagged twice in the #492 plan, now confirmed against a real CRAN package. It is also, on inspection, the *correct* failure. Checked against `15deaf38`, immediately before #492: passing `offset = am` to `fit()` on the formula-to-formula path silently discarded it, and the resulting coefficients are byte-identical to `lm(mpg ~ wt + cyl)` with no offset at all. The vignette has been documenting an offset that was never applied. #492 did not change the model, it exposed that the example was already wrong.

Action sits with tidypredict rather than parsnip: the vignette should either drop `offset` or pass it somewhere it actually takes effect.

### Not attributable

`viralmodels` appears in `revdep/failures.md` with an error before installation, but it fails identically on the CRAN side, so it is an environment problem rather than a regression. The summary correspondingly reports 0 packages failed to check.
