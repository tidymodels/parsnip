# Issue #1407: BART classification prediction interval bounds scrambled across observations

## Issue

- Number: #1407
- Title: BART classification prediction interval bounds scrambled across observations
- URL: https://github.com/tidymodels/parsnip/issues/1407
- Filed: 2026-09-01
- Current labels: `tidy-dev-day :nerd_face:` (no comments on the thread)
- Classification: this is a correctness bug, and arguably should also carry the `bug` label. The scrambling itself produces silently wrong interval bounds; the `rlang::set_names()` error shown in the issue reprex is a second, distinct failure (unused factor level in the outcome) that happens to surface first when the outcome factor is not `droplevels()`ed.

## Symptom

Reproduced live against the current checkout (dbarts installed).

With the issue's exact reprex (outcome keeps the unused `virginica` level), predict errors:

```r
devtools::load_all("/Users/max/github/parsnip")
set.seed(83156)
dat <- iris[iris$Species != "virginica", ]
fit1 <- bart(trees = 5) |>
  set_engine("dbarts") |>
  set_mode("classification") |>
  fit(Species ~ ., data = dat)
predict(fit1, iris[1:5, ], type = "conf_int")
#> Error in `rlang::set_names()` at parsnip/R/bart.R:187:7:
#> ! The size of `nm` (6) must be compatible with the size of `x` (4).
```

After `dat$Species <- droplevels(dat$Species)`, prediction succeeds but the bounds are silently wrong. Observed output for the first five (setosa) rows:

```
  .pred_lower_setosa .pred_lower_versicolor .pred_upper_setosa .pred_upper_versicolor
1              0.952             0.00000843              1.000                  ...
2              0.950             0.00000843              1.000
3              0.934             0.00000907              1.000
4              0.925             0.00000964              1.000
5              0.923             0.00000964              1.000
```

versus the directly computed per-observation quantiles of the posterior (`predict(fit$fit, dat[1:5, ], type = "ev")`, `quantile(probs = c(0.025, 0.975))` per column):

```
             2.5%      97.5%
[1,] 8.434061e-06 0.04974164
[2,] 9.065509e-06 0.07655918
[3,] 9.644559e-06 0.07498221
[4,] 9.644559e-06 0.06556859
[5,] 8.434061e-06 0.04842403
```

The returned `.pred_lower_versicolor` column is exactly the five true 2.5% quantiles sorted ascending (8.43e-06, 8.43e-06, 9.07e-06, 9.64e-06, 9.64e-06) rather than each observation's own quantile: row 2's true lower bound is 9.07e-06 but 8.43e-06 is returned. Each bound column is a sorted pool of all observations' values, so a given row's interval can be built from other observations' posteriors.

## Root cause

`/Users/max/github/parsnip/R/bart.R`, `dbart_predict_calc()`, classification interval branch (lines 184-199 on current main):

```r
bnds <- apply(post_dist, 2, quantile, probs = c(lo, hi), na.rm = TRUE)
bnds <- apply(bnds, 1, function(x) sort(x))
```

- Line 184: `post_dist` is samples x observations; the first `apply()` yields a 2 x n matrix (row 1 = lo quantile, row 2 = hi quantile, one column per observation). This part is correct.
- Line 185: `apply(bnds, 1, sort)` operates on each *row*, i.e. it sorts the lo quantiles across all observations, and separately the hi quantiles across all observations. `apply()` then binds the two length-n results as columns, so `bnds` becomes an n x 2 matrix whose column 1 is the sorted-lo pool and column 2 the sorted-hi pool. The subsequent indexing at lines 189-192 (`bnds[, 1]`, `bnds[, 2]`) is dimensionally valid, which is why the scrambling is silent — each observation just gets someone else's (rank-matched) bounds. The line was presumably intended as a transpose-plus-ordering, but a sort is not a transpose.
- The regression branch directly above (lines 177-182) does it correctly: one `apply(post_dist, 2, quantile, probs = ...)` per bound, indexed per observation.
- The `set_names()` error at line 194 in the issue reprex is a separate problem: `obj$lvl` retains the unused `virginica` level, so `paste0(".pred_lower_", obj$lvl)` produces 6 names for the 4-column tibble. It is not caused by the sort/reshape (with a properly two-level outcome the same code returns without error, as shown above).

## Proposed fix

In `/Users/max/github/parsnip/R/bart.R`, delete line 185 and index the 2 x n quantile matrix by row, matching the style of the regression branch:

```r
bnds <- apply(post_dist, 2, quantile, probs = c(lo, hi), na.rm = TRUE)

res <-
  tibble::tibble(
    .pred_lower_a = 1 - bnds[2, ],
    .pred_lower_b = bnds[1, ],
    .pred_upper_a = 1 - bnds[1, ],
    .pred_upper_b = bnds[2, ]
  ) |>
  rlang::set_names(
    c(
      paste0(".pred_lower_", obj$lvl),
      paste0(".pred_upper_", obj$lvl)
    )
  )
```

No sorting step is needed at all: `quantile()` is monotone in the probability, and `lo < hi` always (lines 155-156), so `bnds[1, ] <= bnds[2, ]` holds per column by construction. The issue text's suggested `apply(bnds, 2, sort)` would also be correct but is redundant; prefer the direct row indexing since it mirrors the regression branch. The level mapping stays consistent with the `class`/`prob` branches (`post_dist` columns are P(second level), so level-a bounds are the complements of level-b bounds, swapped).

The unused-factor-level `set_names()` error is out of scope for this fix (see follow-ups); the repro and test should use a `droplevels()`ed outcome.

## Tests

Add to `/Users/max/github/parsnip/tests/testthat/test-bart.R` (currently only has a `check_args()` test), guarded by `skip_if_not_installed("dbarts")`:

- Fit with a fixed RNG-like seed (e.g. `set.seed(83156)`) on a two-level, `droplevels()`ed subset of iris with `bart(trees = 5)`, engine `dbarts`, mode classification; predict `type = "conf_int"` on a handful of rows.
- `expect_named()` on the result: `.pred_lower_setosa`, `.pred_lower_versicolor`, `.pred_upper_setosa`, `.pred_upper_versicolor`.
- `expect_all_true(res$.pred_lower_versicolor <= res$.pred_upper_versicolor)` and the same for the setosa pair (lower <= upper per row; avoid `expect_true(all(...))` per project convention).
- Exact-match check against directly computed quantiles from the same fitted object: `post <- predict(fit$fit, new_data, type = "ev")`, `q <- apply(post, 2, quantile, probs = c(0.025, 0.975), na.rm = TRUE)`, then `expect_equal(res$.pred_lower_versicolor, unname(q[1, ]))` and `expect_equal(res$.pred_upper_versicolor, unname(q[2, ]))`, plus the complements for the setosa columns. This is deterministic because `type = "ev"` evaluates the stored trees; no RNG is drawn at predict time.
- Optionally a lighter `type = "pred_int"` check (lower <= upper and column names only); `type = "ppd"` draws from the posterior predictive, so exact-match comparison there would require wrapping both predictions in identical `set.seed()` calls — not worth it.

## Follow-ups / risks

- Draft NEWS.md bullet: `bart()` classification confidence and prediction intervals from the dbarts engine now return each observation's own bounds; previously the bounds were sorted across observations, assigning rows the wrong interval limits (#1407).
- Behavior change: interval values will change for anyone who obtained (silently wrong) results before; that is the point of the fix.
- Related latent problems in the same function, worth separate issues rather than scope creep here: (1) an outcome factor with unused (or more than two) levels breaks the interval branch via the `obj$lvl` name mismatch (the error in the issue reprex) and would also mislabel `class`/`prob` predictions — dbarts BART classification is binary-only and parsnip does not check this at fit time; (2) line 175 uses `obj$lv`, which only works through `$` partial matching of `obj$lvl` and would hit the same 3-name/2-column mismatch for non-dropped levels in the `prob` branch.
- No blockers; dbarts is available and the fix is local to `dbart_predict_calc()`.

## Work items

Executed 2026-09-22 on branch `bart-interval-sorts` (off `main` at 11c10db6).

- [x] Reproduce the scrambling and confirm each bound column is a sorted pool
- [x] Drop the `apply(bnds, 1, sort)` line and index the 2 x n quantile matrix by row
- [x] Verify all four columns match the per-observation quantiles exactly
- [x] Confirm the regression branch is untouched
- [x] Tests in `tests/testthat/test-bart.R`, confirmed to fail without the fix
- [x] `NEWS.md` bullet
- [x] `air format .` and full `R CMD check`

### Confirmation of the diagnosis

Reproduced on five setosa rows. The returned `.pred_upper_versicolor` was `0.0484, 0.0497, 0.0656, 0.0750, 0.0766` — exactly the ascending sort of the true per-observation upper quantiles `0.0497, 0.0766, 0.0750, 0.0656, 0.0484`. Row 1's true upper bound is `0.0497` but it was handed row 5's `0.0484`. Same pattern on the lower bounds. After the fix all four columns match the directly computed quantiles to `all.equal()` tolerance.

The plan's reasoning for dropping the sort rather than replacing it with `apply(bnds, 2, sort)` holds: `quantile()` is monotone in the probability and `lo < hi` by construction at lines 155-156, so `bnds[1, ] <= bnds[2, ]` per column already. The tests assert that invariant anyway via `expect_all_true()`.

### Test determinism

The exact-match assertions use `type = "ev"`, which evaluates the stored trees and draws no new samples, so comparing parsnip's output against `apply(post, 2, quantile, ...)` on the same fitted object is deterministic. Verified the four exact-match assertions all fail against unfixed source and pass after, so they genuinely guard the defect rather than merely exercising the code path.

`type = "ppd"` was left alone as the plan suggested — it draws from the posterior predictive, so an exact-match comparison would need matched `set.seed()` calls on both sides for little added value. The `pred_int` path shares the same fixed code, so it is covered by construction.

### Partial matching in the `prob` branch, also fixed

Line 175 used `obj$lv`, which only resolved through `$` partial matching of `obj$lvl`. Fixed on this branch at topepo's request.

Behaviour-neutral today — a `model_fit` has elements `lvl ordered spec fit preproc elapsed censor_probs`, none named `lv`, so the partial match always landed on `lvl`. It was not harmless though: under `options(warnPartialMatchDollar = TRUE)`, which strict CI setups enable, every `type = "prob"` prediction emitted `partial match of 'lv' to 'lvl'`. It would also have started resolving to the wrong element the moment anything named `lv` was added to the object.

Swept all six BART predict paths (`prob`, `class`, `conf_int`, `pred_int`, regression `numeric`, regression `conf_int`) under `warnPartialMatchDollar = TRUE`; all are clean after the change. The guard test runs the four classification types with that option set via `withr::local_options()`, so a regression to `obj$lv` fails the suite rather than passing silently. Confirmed it fails against unfixed source.

No `NEWS.md` bullet: results are unchanged and this is an internal robustness fix, which the project conventions exclude.

### Still open in the same function

Not caused by this fix, and still present:

- An outcome factor with unused levels breaks the interval branch — `obj$lvl` keeps the unused level, so `paste0(".pred_lower_", obj$lvl)` generates more names than the tibble has columns. That is the `rlang::set_names()` error in the original issue reprex, which is why the repro and the tests both `droplevels()`. The underlying assumption is that dbarts BART classification is binary-only and parsnip never checks it at fit time. Worth a separate issue.
