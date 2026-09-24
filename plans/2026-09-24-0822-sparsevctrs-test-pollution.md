# Fix order-dependent failure in `test-sparsevctrs.R`

## Overview

`test-sparsevctrs.R`'s "we don't run as.matrix() on sparse matrix for glmnet pred #1210" passed on its own but failed in a full `devtools::test()` run. It mocked `glmnet::predict.elnet` with `local_mocked_bindings(.package = "glmnet")`, and the mock stopped taking effect once any earlier test file had made a glmnet prediction.

Branch `sparsevctrs-test`, off `main` at d1d22882. No issue number. Test-only, so no `NEWS.md` bullet.

Introduced by `test-glmnet-engines.R`, added for #1069 and #857. The failure never showed up in `R CMD check`, which runs tests against the installed package, so a string of green checks went past it.

## Work items

- [x] Reproduce and bisect to the responsible test file
- [x] Identify the actual mechanism rather than assuming
- [x] Replace the mock with a direct test of the parsnip function that owns the invariant
- [x] Confirm order independence against the previously failing pairing
- [x] Confirm the full suite is clean
- [x] `air format .` and full `R CMD check`

## Root cause: the S3 dispatch cache

Not a binding problem — a caching one. Once `predict.elnet` has been dispatched for real, R caches the resolved method. `local_mocked_bindings()` swaps the namespace binding, but dispatch keeps using the cached original:

```
real prediction done (predict.elnet now dispatched + cached)
getS3method sees mock: TRUE
result: dispatch used the ORIGINAL
```

`getS3method()` reports the mock while `UseMethod()` ignores it. That gap is what made this confusing to diagnose, and it is unfixable from the test side: there is no supported way to invalidate the cache.

Bisecting `test-glmnet-engines.R` matched the mechanism exactly. Its first block only *fits* glmnet models and is harmless; blocks two and three make glmnet *predictions* and both trigger the failure.

This is the hazard the project conventions warn about for the `.package` argument to `local_mocked_bindings()`.

## Fix

The conventions say to mock a function in the current package instead. Better still here, no mock is needed at all: parsnip already owns the decision point.

`organize_glmnet_pre_pred()` (`R/glmnet-engines.R:104-111`) builds the `newx` that glmnet receives, returning the matrix untouched when it is sparse and calling `as.matrix()` otherwise. That *is* the invariant #1210 is about, so the test now exercises it directly:

- a sparse input stays sparse, with columns subset to the fitted coefficients
- a dense input stays a dense matrix
- the registered prediction module really does route `newx` through it, asserted against `get_from_env("linear_reg_predict")`, so the unit test cannot pass while the wiring is broken
- `predict()` on sparse data still runs without error

That last assertion is what the old test was reaching for; the first three are what it was actually trying to prove. No mocking, no namespace modification, no ordering assumptions.

## Verification

| | before | after |
|---|---|---|
| `test-sparsevctrs.R` alone | 30 pass | 35 pass |
| `^(glmnet-engines\|sparsevctrs)` | **1 failure** | 55 pass |
| full `devtools::test()` | **1 failure** | no failures |

The obsolete snapshot entry was pruned as a consequence: an 8-line deletion, verified to be only the `#1210` block with no other entries touched. That file has a history of over-eager pruning, so the diff was checked rather than assumed.
