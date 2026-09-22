# Issues #1413 and #1415: LiblineaR 2.10-25 breaks the hardcoded regression prediction references

## Issues

- [#1413](https://github.com/tidymodels/parsnip/issues/1413), "LiblineaR update breakage", filed 2026-09-10. The upstream maintainer's advance warning, quoted from their CRAN submission.
- [#1415](https://github.com/tidymodels/parsnip/issues/1415), "`svm_linear()` LiblineaR regression prediction test fails with LiblineaR 2.10-25", filed 2026-09-17. The tracking issue opened when `test-coverage` CI failed in PR #1414; this is the issue the `skip()` points at.

Same defect from two directions. Both are closed by this change.

#1415 asks whether the change is an intentional upstream algorithm change (update the values) or a regression worth reporting upstream. #1413 answers it: intentional. The LiblineaR maintainer fixed a bug in which `epsilon` silently defaulted to 0.01 for every solver type rather than LIBLINEAR's documented per-solver default. Nothing to report upstream.

## Overview

LiblineaR 2.10-25 fixed a bug in which `LiblineaR()`'s `epsilon` (solver convergence tolerance) silently defaulted to 0.01 for every solver type instead of LIBLINEAR's documented per-solver default. The `svm_linear()` LiblineaR regression engine uses `type = 11` without setting `epsilon`, so it now converges under the real default of 0.001. On the `hpc` data used in `tests/testthat/test-svm_linear.R` this is a materially different fit, and the hardcoded `liblinear_pred` values no longer match within `tolerance = 0.0001`. CRAN's reverse-dependency pretest for the LiblineaR submission flagged it as a "change to worse".

The stopgap was commit 5273f0fb, which added `skip("fix this later")` to `linear svm regression prediction: LiblineaR` and pointed at #1415. That left the LiblineaR regression prediction path with no coverage at all.

Fixed on branch `liblinear-version` alongside #1405, since both land in the same test file and the same engine. See [issue 1405](2026-09-09-issue-1405-liblinear-cost-arg.md).

## Work items

- [x] Confirm the drift is the LiblineaR `epsilon` change and not the #1405 `cost` fix
- [x] Replace the hardcoded `liblinear_pred` values with a reference computed from a direct `LiblineaR::LiblineaR()` call
- [x] Remove `skip("fix this later")`
- [x] Drop the unused `hpc_no_m` binding from the test
- [x] `air format .` and run the file's tests

## Root cause of the fragility

The test asserted LIBLINEAR's numeric output, not parsnip's plumbing. Any upstream solver change — even a bug fix, as here — breaks it, and it breaks in the most expensive possible place: a CRAN reverse-dependency pretest for someone else's submission.

## Fix

`tests/testthat/test-svm_linear.R`, in `linear svm regression prediction: LiblineaR`: build the expected values by calling the engine directly with the same arguments parsnip translates to, then compare parsnip's `predict()` output against that. Both the formula fit and the `fit_xy()` fit are checked against the same reference. The direct call is wrapped in `suppressWarnings()` because the engine warns that no `svr_eps` was supplied and is falling back to its default; parsnip makes the identical call and silences the same warning through `control_parsnip(verbosity = 0)`.

`svr_eps` is deliberately *not* passed explicitly. Letting the reference call inherit the engine default is what keeps parsnip and the reference in lockstep if that default ever changes again.

Verified: the reference fit's `W` is identical to `extract_fit_engine()` on the parsnip fit, and predictions agree exactly (116.9538, 592.97262, 1862.88507 at rows 2, 1, 143).

## Evidence that #1405 is not the cause

| | values at `hpc` rows 2, 1, 143 |
|---|---|
| hardcoded in the skipped test | 85.13979, 576.16232, 1886.10132 |
| parsnip after the #1405 fix (`cost = 1/4`) | 116.9538, 592.97262, 1862.88507 |
| engine at `cost = 1`, i.e. pre-#1405 behavior | 116.95434, 592.97292, 1862.8847 |

`cost` barely moves the type-11 solver on this data — a relative difference of roughly 5e-6, well inside the test's `tolerance = 0.0001`. The #1405 fix alone would not have broken this test, and does not fix this one.

## Follow-ups / risks

- No `NEWS.md` bullet: this is a test-only change with no user-facing effect.
- No source changes, so no re-documentation or re-knitting needed.
- The same hardcoded-engine-output pattern exists elsewhere in the test suite (for example the kernlab reference values further down `test-svm_linear.R`, which are still literals). Those have not broken, but they carry the same fragility and are worth converting the next time one of them fails. This overlaps with #1195 (stop snapshot-testing base R error text) in spirit but is a separate sweep.
- The test file no longer skips anything: `devtools::test(filter = "^svm_linear")` reports `FAIL 0 | WARN 0 | SKIP 0 | PASS 41`.
