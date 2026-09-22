# Issue #1405: LiblineaR `cost` parameter mapped to wrong argument name, silently ignored during tuning

## Issue

- Number: #1405
- Title: LiblineaR `cost` parameter mapped to wrong argument name, silently ignored during tuning
- URL: https://github.com/tidymodels/parsnip/issues/1405
- Filed: 2026-09-01 (no comments on the thread)
- Current labels: `tidy-dev-day :nerd_face:`
- Classification: a correctness bug; it should arguably also carry the `bug` label. Every `cost` value a user sets (including every point in a tuning grid) is silently discarded and the engine default (`cost = 1`) is used instead.

## Symptom

Reproduced live against the current checkout (LiblineaR installed). The translated template passes `C = 1`, but `LiblineaR::LiblineaR()` has no `C` formal — its argument is `cost` — and because `LiblineaR()` has `...`, the bogus `C` is silently swallowed:

```r
devtools::load_all("/Users/max/github/parsnip")
translate(svm_linear(cost = 1) |> set_engine("LiblineaR") |> set_mode("classification"))
#> Model fit template:
#> LiblineaR::LiblineaR(x = missing_arg(), y = missing_arg(), C = 1,
#>     type = 1)

names(formals(LiblineaR::LiblineaR))
#>  [1] "data"     "target"   "type"     "cost"     "epsilon"  "svr_eps"
#>  [7] "bias"     "wi"       "cross"    "verbose"  "findC"    "useInitC"
#> [13] "..."
```

Two very different cost values produce byte-identical fits through parsnip (the solver has internal randomness, so the RNG seed is fixed for the comparison), while direct engine calls with `cost` differ substantially:

```r
set.seed(1); a <- fit(spec(0.001), y ~ ., data = dat)  # svm_linear(cost = 0.001), LiblineaR engine
set.seed(1); b <- fit(spec(1000),  y ~ ., data = dat)
identical(a$fit$W, b$fit$W)
#> [1] TRUE      # tuning over cost is a no-op

set.seed(1); d_lo <- LiblineaR::LiblineaR(x, dat$y, type = 1, cost = 0.001)
set.seed(1); d_hi <- LiblineaR::LiblineaR(x, dat$y, type = 1, cost = 1000)
identical(d_lo$W, d_hi$W)
#> [1] FALSE
#> d_lo$W: -0.0278 -0.0196 (Bias -0.0064)   d_hi$W: -0.8486 -0.4871 (Bias 0.1912)
```

## Root cause

`/Users/max/github/parsnip/R/svm_linear_data.R:13-20` registers the LiblineaR engine's `cost` with the wrong `original` name:

```r
set_model_arg(
  model = "svm_linear",
  eng = "LiblineaR",
  parsnip = "cost",
  original = "C",
  func = list(pkg = "dials", fun = "cost", range = c(-10, 5)),
  has_submodel = FALSE
)
```

`translate()` renames parsnip's `cost` to the registered `original` when building the fit template, producing `C = 1` in the `LiblineaR::LiblineaR()` call. Since `LiblineaR()` accepts `...`, R raises no "unused argument" error; the value lands in the dots, is never read, and `cost` falls back to the engine default of 1.

The other three `original = "C"` registrations in the codebase are correct, because those engines really do have a `C` formal (`kernlab::ksvm()`):

- `/Users/max/github/parsnip/R/svm_linear_data.R:149-156` (kernlab)
- `/Users/max/github/parsnip/R/svm_poly_data.R:17` (kernlab)
- `/Users/max/github/parsnip/R/svm_rbf_data.R:17` (kernlab)

The second LiblineaR arg, `margin` with `original = "svr_eps"` (lines 22-29), matches a real formal and is fine.

## Proposed fix

Change `original = "C"` to `original = "cost"` at `/Users/max/github/parsnip/R/svm_linear_data.R:17` only. (The task description also mentions line 152-153, but that block is the kernlab engine, where `original = "C"` is correct and must stay.)

Nothing else needs code changes:

- `tunable()` metadata keys off the parsnip name (`cost`) and the `func` mapping to `dials::cost`, both unchanged (verified live: `tunable()` returns `cost` and `margin` rows).
- `has_submodel = FALSE`, so there is no `min_grid`/`multi_predict()` submodel machinery to touch (no svm `min_grid` methods exist in parsnip).
- Sweep for other instances of the same bug class: for every row of the model-environment arg tables (`get_model_env()`), compare `original` against `names(formals())` of the function registered via `set_fit()` for that model/engine/mode. Flag any `original` not among the formals — including when the engine function has `...`, since dots-swallowing is exactly what made this failure silent. A one-off audit script (not shipped) can loop over `get_from_env(paste0(model, "_args"))` and the corresponding `set_fit()` `func`; flagged rows need manual review because some engines intentionally route arguments through `...` to an inner function (e.g. `xgb_train()`-style wrappers).
- Re-knit the LiblineaR engine docs, which currently show the wrong template (`C = double(1)` at `/Users/max/github/parsnip/man/rmd/svm_linear_LiblineaR.md:43,68` and `/Users/max/github/parsnip/man/details_svm_linear_LiblineaR.Rd:49,72`). Per the project memory note, knitting engine docs requires deregistering tabby's duplicate registrations in-session and then `library(tabby)`.

## Tests

In `/Users/max/github/parsnip/tests/testthat/test-svm_linear.R` (snapshots in `tests/testthat/_snaps/svm_linear.md`):

- Add a translate check for the LiblineaR engine, e.g. `expect_snapshot(print(translate(svm_linear(cost = 1) |> set_engine("LiblineaR") |> set_mode("classification"))))`, asserting the template contains `cost = 1` and no `C`. A non-snapshot companion can use `expect_named()` on `translate(...)$method$fit$args` with `c("x", "y", "cost", "type")` (partial-match-proof, unlike checking `$args$C`).
- The existing `_snaps/svm_linear.md` has no LiblineaR translate snapshot today (only a kernlab `update()` snapshot), so no snapshot *edits* are needed for translate — but new snapshots will be recorded.
- Existing hardcoded reference predictions will change and must be regenerated, because `reg_mod` (`cost = 1/4`, line 38) and `cls_mod` (`cost = 1/8`, line 43) previously fit with the default cost of 1: `liblinear_pred` (lines 96-101, checked at 103-108 and 122-127) and the classification checks (`liblinear_class` at lines 177-188, plus the `W` equality and prob-error snapshots). Regenerate the reference values with direct `LiblineaR::LiblineaR(..., cost = 1/4)` / `cost = 1/8` calls so they stay independent of parsnip.

## Follow-ups / risks

- Draft NEWS.md bullet: `svm_linear()` now correctly passes `cost` to `LiblineaR::LiblineaR()`; previously the value was mapped to a nonexistent `C` argument and silently ignored, so all user-supplied or tuned cost values fit with the engine default of 1 (#1405).
- Behavior change: fitted results change for every existing `svm_linear()`/LiblineaR model with a non-default `cost`, and past tuning results over `cost` with this engine were meaningless (all candidates were identical fits up to solver randomness). Worth an explicit NEWS callout.
- Run the arg-name audit described above across all built-in engine data files and file separate issues for any other mismatches found.
- No blockers; the code fix is one word, with test-reference regeneration and engine-doc re-knitting as the bulk of the work.

## Work items

Executed 2026-09-22 on branch `liblinear-version` (off `main` at 8ca7373e).

- [x] Change `original = "C"` to `original = "cost"` in `R/svm_linear_data.R:17` (LiblineaR only; the kernlab block at line 153 keeps `"C"`)
- [x] Confirm `translate()` now emits `cost =` for both LiblineaR modes
- [x] Confirm a parsnip fit is byte-identical to a direct `LiblineaR::LiblineaR(..., cost = 1/8)` call, and differs from the `cost = 1` default
- [x] Add a `translate: LiblineaR` snapshot test covering regression (`cost` + `svr_eps`) and classification
- [x] Add a `cost reaches the LiblineaR engine` regression test comparing the fitted `W` against a direct engine call
- [x] Re-knit `man/rmd/svm_linear_LiblineaR.md` and re-document `man/details_svm_linear_LiblineaR.Rd`
- [x] Add the `NEWS.md` bullet
- [x] Run the `original`-vs-formals audit across all built-in engine registrations
- [x] `air format .` and full `devtools::test()`

### Notes from execution

The plan expected the hardcoded reference predictions in `test-svm_linear.R` to change. They did not: `liblinear_class` (`c(1L, 1L, 2L)` → VF, VF, F) is the same at `cost = 1/8` as it was at the engine default of `cost = 1`, so that test passed unchanged. It is therefore not a guard against this bug at all, which is why the new `cost reaches the LiblineaR engine` test compares the fitted coefficient matrix against a direct engine call instead of comparing predictions.

The `liblinear_pred` regression references sit inside a test that was `skip("fix this later")` — which turned out to be the stopgap for #1413, added in commit 5273f0fb. That was fixed on this branch too; see [issue 1413](2026-09-22-1200-liblinear-epsilon-test-references.md). The two issues are independent: `cost` moves the type-11 regression solver by a relative ~5e-6 on this data, inside the test's own tolerance, so #1405 neither caused nor fixed #1413.

The full `devtools::test()` run ends at `FAIL 1 | WARN 0 | SKIP 8 | PASS 1785`. The one failure is `test-model_basics.R:6:3`, where `print(bart())` prints `Call: NULL` instead of the usual specification output. It is unrelated to this change and pre-existing: it reproduces on a clean tree with these changes stashed, and it does not reproduce when `test-model_basics.R` is run on its own, so it is test pollution from a package attached earlier in the full run. Worth a separate issue.

Knitting the engine docs did not need the tabby workaround. `devtools::load_all()` followed by `knitr::knit("man/rmd/svm_linear_LiblineaR.Rmd", ...)` from the package root produced a two-line diff and nothing else; `devtools::document()` then rewrote only `details_svm_linear_LiblineaR.Rd`, with no `NAMESPACE` churn.

### Audit result

The audit compares every registered `original` name against the union of formals of the `set_fit()` function and its S3/S4 methods. After the LiblineaR fix it reports no genuine mismatch. Every remaining flag was manually confirmed to be deliberate dots-forwarding to a real formal of an inner function:

| Model / engine | Flagged `original` | Real destination |
|---|---|---|
| `boost_tree`, `decision_tree` / C5.0 | `CF`, `noGlobalPruning`, `winnow`, `fuzzyThreshold`, `bands` | `C50::C5.0Control()` formals |
| `boost_tree` / xgboost | `alpha`, `lambda`, `scale_pos_weight` | xgboost `params` list via `xgb_train()` dots |
| `decision_tree` / rpart | `cp`, `minsplit`, `maxdepth` | `rpart::rpart.control()` formals |
| `mars` / earth | `nk` | `earth:::earth.fit()` formal, reached through `earth.default()` dots |
| `mlp` / qrnn | `n.hidden`, `n.hidden2`, `penalty`, `iter.max`, `Th` | `qrnn::mcqrnn.fit()` formals |
| `mlp` / qrnn | `alpha`, `iterbreak`, `minibatch` | `qrnn::adam()` formals, reached through `mcqrnn.fit()` dots |
| `multinom_reg` / nnet | `decay` | `nnet:::nnet.default()` formal |
| `svm_linear`, `svm_poly`, `svm_rbf` / kernlab | `C`, `sigma`, `degree`, `scale`, `epsilon` | documented `kernlab::ksvm()` arguments; all its S4 methods declare a bare `(x, ...)` signature, so formals-based checking cannot resolve them |

`svm_rbf` / liquidSVM could not be checked because the package is not installed. No follow-up issues to file.
