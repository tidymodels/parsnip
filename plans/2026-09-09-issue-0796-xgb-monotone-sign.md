# Issue #796: XGBoost monotone constraint factor levels reversed

## Issue

- Number: #796
- Title: XGBoost Monotone Constraint Factor Levels Reversed
- URL: https://github.com/tidymodels/parsnip/issues/796
- Filed: 2022-08-18 (open)
- Current labels: `feature`
- Classification: this is a documentation gap with a silent-misfit consequence rather than a pure feature request. The `feature` label is defensible only if the fix is limited to a warning plus docs (option (a) below); if the maintainers decide user-supplied constraint signs should follow the conventional 0/1 outcome coding (option (b)), it is a bug because the current behavior silently fits the opposite of what most users intend. The reporter himself said "I don't think this is really a bug per se, but it'd just be nice to let the user know." Emil questioned the reporter's `"(1)"` string syntax; the reporter confirmed that plain numeric `1`/`-1` reproduces the behavior, and there has been no follow-up since 2022.

## Symptom

For binary classification with the default `event_level = "first"`, a user-supplied `monotone_constraints` engine argument acts on xgboost's internal label coding, in which the FIRST factor level is 1. Users who think of the constraint as acting on the second level ("positive class" in 0/1 coding, or the reporter's `"yes"`) see the constraint applied in the reverse direction. In the reporter's reprex, a `+1` constraint on a feature with a clearly positive relationship to the second level produced a decreasing predicted-probability curve.

Minimal live repro (run against this checkout with xgboost 3.2.1.1; `x` truly increases `P(cls == "yes")`, where `"yes"` is the second factor level):

```r
devtools::load_all('/Users/max/github/parsnip')
set.seed(9174)
n <- 300
x <- runif(n)
prob_yes <- 1 / (1 + exp(-(8 * x - 4)))
dat <- data.frame(
  x = x,
  cls = factor(ifelse(runif(n) < prob_yes, 'yes', 'no'), levels = c('no', 'yes'))
)
grid <- data.frame(x = seq(0.05, 0.95, by = 0.30))
for (constraint in c(1, -1)) {
  spec <- boost_tree(trees = 25) |>
    set_mode('classification') |>
    set_engine('xgboost', monotone_constraints = constraint)
  fitted <- fit(spec, cls ~ x, data = dat)
  print(cbind(grid, predict(fitted, grid, type = 'prob')))
}
```

Observed output:

```
monotone_constraints = 1
     x  .pred_no .pred_yes
1 0.05 0.4833333 0.5166667
2 0.35 0.4833333 0.5166667
3 0.65 0.4833333 0.5166667
4 0.95 0.4833333 0.5166667
monotone_constraints = -1
     x   .pred_no  .pred_yes
1 0.05 0.97493535 0.02506465
2 0.35 0.83322030 0.16677970
3 0.65 0.08355386 0.91644614
4 0.95 0.04293055 0.95706945
```

With `+1`, the constraint fights the data so hard that the model degenerates to a constant fit; the user must pass `-1` to get an increasing `.pred_yes`. Both outcomes are silent.

## Root cause

- `R/boost_tree.R:522-535` (`as_xgb_data()`): for a two-level factor outcome, the label is recoded as `y <- -as.numeric(y) + 2` when `event_level == "first"` (the default), so the FIRST factor level becomes xgboost's label 1. With `event_level == "second"`, `y <- as.numeric(y) - 1`, so the second level is label 1.
- `R/boost_tree.R:276` and `R/boost_tree.R:342-362` (`xgb_train()`): `monotone_constraints` arrives via `...` into `others` and is passed through to `xgb.train()` untouched (for xgboost >= 2.0 it is moved into the `params` list at lines 355-362 because it matches `xgboost::xgb.params` formals).
- xgboost interprets `monotone_constraints = 1` as "the margin for label 1 is nondecreasing in this feature." Under the default recoding, that margin is the log-odds of the FIRST factor level, so `+1` means "probability of the first level increases," i.e., "probability of the second level decreases."

Notably, the current behavior is internally consistent with parsnip's event conventions: under both `event_level = "first"` and `event_level = "second"`, a `+1` constraint means the probability of the event level increases with the feature. The problem is that this is undocumented and contradicts the 0/1-coding intuition (`label = as.numeric(y) - 1`) that users bring from raw xgboost, glm, etc. Nothing warns them.

## Proposed fix

Recommended: option (a), warn plus document.

- In `xgb_train()` (`R/boost_tree.R`, after `others <- process_others(others, arg_list)` at line 342, which also catches constraints smuggled in via a deprecated `params` list), when `others$monotone_constraints` is not `NULL` and `y` is a two-level factor, emit a `cli::cli_warn()` stating that constraint signs are interpreted relative to the event level (the first factor level unless `event_level = "second"` was supplied), so `+1` means the probability of the event level is nondecreasing in that feature. Give the warning a class (e.g., `"xgboost_monotone_direction_warning"`) so tests can snapshot it and downstream tools can suppress it.
- Document the convention in the "Other details" section of `man/rmd/boost_tree_xgboost.Rmd` (the existing `event_level` paragraph, currently rendered at `man/rmd/boost_tree_xgboost.md:153`, is the natural anchor), with a one-sentence example: with default settings and levels `c("no", "yes")`, `monotone_constraints = 1` forces `P(no)` to be nondecreasing.

Reasons to prefer (a): the current behavior is a coherent convention ("+1 increases P(event)" under either `event_level`), the reporter explicitly did not consider it a bug, and option (b) silently changes fitted models for every existing pipeline that empirically calibrated its signs against the current behavior.

Alternative: option (b), flip constraint signs alongside the outcome recoding.

- In `xgb_train()`, when `y` is a two-level factor and `event_level == "first"` (the only case where the label is inverted), negate the user's constraints before they reach `xgb.train()`, making `+1` always mean "probability of the SECOND factor level increases" (matching `label = as.numeric(y) - 1` intuition). `event_level = "second"` and multiclass must not flip.
- The flip helper must handle every form xgboost accepts: an unnamed numeric vector (negate, `0` stays `0`), a named numeric vector (negate values, preserve names), and the string form `"(1,0,-1)"` (parse, negate, rebuild). Do this in `others` before the xgboost >= 2.0 branch at line 344 so both the `arg_list`/`params` path and the legacy path see the flipped values.
- This is a behavior change that alters model fits; it would need a prominent NEWS entry and probably a deprecation-style transition (warn once that directions changed).

If maintainers want (b), also decide whether the docs should describe the new invariant ("+1 increases P(second level), regardless of `event_level`") — under (b) the constraint is no longer tied to the event level.

## Tests

Add to `tests/testthat/test-boost_tree_xgboost.R`, next to the existing `"fit and prediction with `event_level`"` test (line 684):

- For option (a): a test that fitting `boost_tree() |> set_mode("classification") |> set_engine("xgboost", monotone_constraints = 1)` on a small two-level-factor dataset produces the warning, captured with `expect_snapshot()` (per project convention; snapshot goes in `tests/testthat/_snaps/boost_tree_xgboost.md`). Also test that no warning fires for regression or when `monotone_constraints` is absent, and that the fit still succeeds (check the fitted params with the existing `extract_xgb_param()` helper pattern used at line 252).
- For option (b): a deterministic direction test — fit the repro data above with `monotone_constraints = 1` and assert that `.pred_yes` at a high `x` value is greater than at a low `x` value (avoid `expect_true()`; compare with `expect_gt()`), plus the same for `event_level = "second"` un-flipped, and a unit test of the flip helper on named vectors and the `"(1,0,-1)"` string form using `expect_identical()`.
- All tests need `skip_if_not_installed("xgboost")` and `skip_on_cran()` like their neighbors.

## Follow-ups / risks

- Draft NEWS bullet (option a): `xgb_train()` now warns when `monotone_constraints` is supplied for binary classification, because constraint signs apply to the event level (the first factor level by default); the xgboost engine documentation now describes this convention (#796).
- Draft NEWS bullet (option b, if chosen instead): `xgb_train()` now flips user-supplied `monotone_constraints` signs for binary classification with `event_level = "first"` so that `+1` constrains the probability of the second factor level to be nondecreasing, matching standard 0/1 outcome coding; existing fits that relied on the previous direction will change (#796).
- Option (a) will warn on every fit during tuning; tune collects and de-duplicates warnings, but the noise is a real cost — an alternative is `cli::cli_inform()` or warning only once per session (`rlang::warn(..., .frequency = "once")`).
- Regenerating `man/rmd/boost_tree_xgboost.md` from the `.Rmd` requires the engine-docs knit workflow (deregister tabby's duplicate engine registrations in-session first, per the project memory note).
- Related: the same event-level label inversion affects any other xgboost `params` entry that is asymmetric in the label (e.g., `scale_pos_weight`); worth an audit but out of scope here.
