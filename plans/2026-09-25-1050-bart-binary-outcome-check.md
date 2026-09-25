# Issue #1444: error on non-binary outcomes for `bart()` and `logistic_reg()`

## Overview

`bart()` classification with the `"dbarts"` engine accepted an outcome with any number of levels. dbarts models a *binary* outcome, and parsnip handed it the factor codes without checking, so anything other than exactly two levels produced invalid results.

- Issue: [#1444](https://github.com/tidymodels/parsnip/issues/1444)
- Branch: `bart-unused-levels`, off `main` at bd62b798
- Found while fixing [#1407](2026-09-09-issue-1407-bart-interval-bounds.md), which flagged the `set_names()` error as out of scope

## Work items

- [x] Reproduce across every prediction type and every shape of bad outcome
- [x] Trace the root cause to the fit rather than the prediction path
- [x] Add the check to `check_outcome()` in `R/misc.R`
- [x] Distinguish the unused-level case, where `droplevels()` is the fix
- [x] Tests in `tests/testthat/test-bart.R`, confirmed to fail without the fix
- [x] File the issue and add a `NEWS.md` bullet
- [x] Extend the same rule to `logistic_reg()` at topepo's request: error on 3+ levels rather than warn
- [x] Survey every logistic engine before erroring, and carve out `LiblineaR`
- [x] File the `multinom_reg()` LiblineaR follow-up (#1445)
- [x] Refactor `check_outcome()` into per-mode helpers plus a per-model generic
- [x] `air format .` and full `R CMD check`

## Root cause

At fit time, not predict time. dbarts receives the factor codes directly:

```r
fit3 <- bart(trees = 5) |> set_engine("dbarts") |> set_mode("classification") |>
  fit(Species ~ ., data = iris)
sort(unique(fit3$fit$y))
#> [1] 0 1 2
```

dbarts expects `0`/`1`. Given `0`/`1`/`2` it fits something meaningless, and by predict time the model is already garbage — which is why this had to be caught at fit.

## Three distinct symptoms, one cause

| Outcome | Symptom |
|---|---|
| 3 used levels | `prob` returns values outside `[0, 1]` (observed `-1.002`, `2.002`), only two columns for three levels, and the third level missing while the columns are mislabelled |
| 2 used + unused level **first** | codes become `1`/`2` rather than `0`/`1`; `prob` again outside `[0, 1]`, `class` predictions wrong |
| 2 used + unused level **last** | codes are `0`/`1` so the fit is fine, but `obj$lvl` has three entries, so `conf_int`/`pred_int` error inside `rlang::set_names()` with "The size of `nm` (6) must be compatible with the size of `x` (4)" |

The last row is the error originally reported in #1407. It is the most visible symptom but the least harmful — the other two are silent.

## Fix

A check in `check_outcome()` (`R/misc.R`), which receives both `y` and the spec and already contains the closely analogous `logistic_reg` + more-than-two-levels case immediately below.

This **errors** rather than warns: there is no reading under which a probability of `2.002` was intended. The `logistic_reg` precedent below warned rather than erroring, but that was revisited in the same pass — see the extension section.

The message distinguishes the two situations, because they have different fixes. When exactly two levels are present in the data it names the unused ones and suggests `droplevels()`; when more than two are genuinely used it does not, since dropping nothing would help:

```
! BART classification with the "dbarts" engine requires an outcome with
  exactly 2 levels, but 3 were given: "setosa", "versicolor", and "virginica".
i dbarts models a binary outcome, so additional levels cannot be fit.
i Only "setosa" and "versicolor" appear in the data.
i Use `droplevels()` to drop the unused level "virginica".
```

Scoped to `bart` + `"dbarts"` rather than to `bart` alone. That engine is currently the only one registered for the model, but keying on it avoids blocking a future engine that does support multiclass.

### A cli detail worth remembering

The `droplevels()` hint first rendered as "the unused **levels** virginica" for a single level. `cli` resolves `{?s}` against the most recently set quantity, and the `{.val {used}}` interpolation sitting between `cli::qty(unused)` and `level{?s}` reset it to the length of `used`. Splitting the sentence into two bullets, so nothing intervenes, fixes it. Both singular and plural were then verified.

## Verification

- Clean binary outcomes are unaffected: fit succeeds, all four prediction types work, and probabilities are within `[0, 1]`.
- BART regression is unaffected.
- `multinom_reg()` with three levels is unaffected.
- All three new snapshot assertions fail against unfixed source, so they are real guards.

`tests/testthat/_snaps/bart.md` is a new file — `test-bart.R` had no snapshots before.

## Extension: `logistic_reg()` errors instead of warning

topepo asked for the same rule on logistic models. `check_outcome()` already warned there — added in 1.1.0 (#545) — with the comment that it warned "since some engines handle this case by binning all but the first level as the non-event, so this may be intended".

That claim was tested rather than taken at face value, and it does not hold uniformly:

| Engine | 3-level outcome today |
|---|---|
| `glm` | fits, bins levels 2+ into the event, and parsnip labels the second column `.pred_<level 2>` when it is really P(not level 1) — silently wrong |
| `keras3` | returns a column per level, but the class predictions are wrong — silently wrong |
| `stan` | errors, with an internal `Bug found: factor with nlevels != 2` message |
| `glmnet` | errors cleanly: "More than two classes; use multinomial family instead" |
| `brulee` | errors cleanly: "Logistic regression is for outcomes with two classes" |
| `LiblineaR` | **fits a correct multiclass model** — 3 columns, rows sum to 1, all classes predicted correctly |

So erroring is right for `glm`, `keras3` and `stan`, harmless for `glmnet` and `brulee`, and wrong for `LiblineaR`.

### The LiblineaR carve-out

`LiblineaR` is exempt. It is the one engine that produces a correct answer, and `multinom_reg()` has no LiblineaR engine, so the error's natural advice — use `multinom_reg()` — would not be available to those users. Erroring would have deleted working functionality with nowhere to go.

topepo chose the carve-out over erroring uniformly. [#1445](https://github.com/tidymodels/parsnip/issues/1445) tracks registering `multinom_reg("LiblineaR")`; once that exists the exemption can be removed and the rule becomes uniform.

The `is.atomic(y)` guard is retained from the original check, so the existing `check_outcome(lapply(mtcars, as.factor), class_spec)` case — a list rather than a vector — is unaffected.

## Note for the release

Two behaviour changes here, not one. Code that previously fit a non-binary BART classification model, or that fit `logistic_reg()` on a 3+ level outcome and merely got a warning, will now error. Both were producing unusable output for the affected engines, so erroring is the safe direction, but both belong with the other breaking changes in this cycle rather than reading as routine bug fixes.


## Refactor: keeping `check_outcome()` short

Adding the two checks inline pushed `check_outcome()` to ~118 lines, and the growth pattern was bad: each new model with an outcome constraint would add another `if (inherits(spec, ...))` block to a widening chain. topepo asked for modularity, so the function was restructured rather than merely shortened.

Two changes:

**Split by mode.** `check_outcome()` is now a dispatcher — it returns early for an unknown mode, calls `check_outcome_exists()`, then switches to one of `check_outcome_numeric()`, `check_outcome_factor()` or `check_outcome_surv()`. Each helper holds one mode's rules and nothing else.

**Per-model checks became an S3 generic.** `check_outcome_levels(spec, y, call)` dispatches on the spec, with a no-op `.default`. The two model-specific checks moved out of `R/misc.R` entirely and now live beside each model's `check_args()` method — `check_outcome_levels.bart()` in `R/bart.R`, `check_outcome_levels.logistic_reg()` in `R/logistic_reg.R`.

This mirrors parsnip's existing `check_args()` hook exactly: an exported generic documented under `add_on_exports`, `@keywords internal`, with methods registered per model. A new model with an outcome constraint now adds a method in its own file, and `check_outcome()` does not grow at all. Extension packages can do the same, which they could not with an `inherits()` chain internal to parsnip.

The `is.atomic(y)` guard was hoisted into `check_outcome_factor()` before the generic is called, so individual methods can use `nlevels()` directly and the existing list-valued outcome case still short-circuits.

Behaviour is unchanged. The full suite passes and the only snapshot movement is the two additions for the new logistic test — 18 insertions, no deletions — confirming every pre-existing error message is byte-identical after the move.
