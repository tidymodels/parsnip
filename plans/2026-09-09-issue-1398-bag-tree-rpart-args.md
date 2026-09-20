# Issue #1398: Move arg registration for `bag_tree()`'s `rpart` engine to parsnip from baguette

## Issue

- Number: #1398
- Title: Move arg registration for `bag_tree()`'s `rpart` engine to parsnip from baguette
- URL: https://github.com/tidymodels/parsnip/issues/1398
- Filed: 2026-07-23 (by maintainer hfrick; confirmed, with a full reprex in the issue body)
- Current labels: `bug`
- Classification: `bug` is right for the user-visible symptom (a bagged survival tree cannot be tuned at all), but the fix is part design work: the `bag_tree`/`rpart` engine is registered by two extension packages for different modes (baguette: classification + regression; censored: censored regression) while the shared, mode-independent argument registrations live only in baguette. hfrick: "the shared part of that registration (i.e. things that are not mode-specific) should go into parsnip." The complication is that parsnip's registry has no concept of mode-specific arguments (related design issue #1351, prior symptom tidymodels/extratests#186).

## Symptom

Reproduced live against the current checkout (censored and baguette both installed here):

```r
devtools::load_all("/Users/max/github/parsnip")
library(censored)   # baguette NOT loaded

spec <- bag_tree(
  cost_complexity = hardhat::tune(),
  tree_depth = hardhat::tune(),
  min_n = hardhat::tune()
) |>
  set_engine("rpart") |>
  set_mode("censored regression")

generics::tunable(spec)
#> # A tibble: 0 x 5
#> # i 5 variables: name <chr>, call_info <list>, source <chr>, component <chr>,
#> #   component_id <chr>

library(tune)
hardhat::extract_parameter_set_dials(spec)
#> Collection of 0 parameters for tuning
#>
#> [1] identifier type       object
#> <0 rows> (or 0-length row.names)

library(baguette)   # "fixes" it, which is the tell
nrow(hardhat::extract_parameter_set_dials(spec))
#> [1] 3
```

So the same spec has zero tunable parameters until baguette happens to be attached — exactly the load-order trap that bit extratests (tidymodels/extratests#186 works around it by loading baguette in censored tests).

## Root cause

Ownership of the registration is split across three packages:

- `/Users/max/github/parsnip/R/bag_tree.R:101-104` — parsnip owns the model: `set_new_model("bag_tree")` plus the three modes (classification, regression, censored regression). Parsnip registers no engines and no arguments for `bag_tree` (verified: `R/bag_tree_rpart.R` and `R/bag_tree_C5.0.R` are documentation-only files; `inst/models.tsv:9-12` lists all four engine/mode rows as owned by censored/baguette).
- baguette `R/bag_tree_data.R` (lines 17-50 at ref 237f2e6) — registers the `rpart` args: `class_cost -> cost`, `tree_depth -> maxdepth`, `min_n -> minsplit`, `cost_complexity -> cp` (and for `C5.0`: `class_cost -> cost`, `min_n -> minCases`). Verified live by loading baguette and printing `get_from_env("bag_tree_args")`.
- censored `R/bag_tree-data.R` — registers the `rpart` engine for mode "censored regression" plus fit/predict modules, but no `set_model_arg()` calls.

Why that yields zero parameters: `tunable.model_spec()` (`/Users/max/github/parsnip/R/tunable.R:24-25`) reads `get_model_env()[["bag_tree_args"]]` and filters by engine only. With only censored loaded, that table is empty, so `tunable()` returns 0 rows; main args whose `call_info` is `NULL` are then dropped at `R/tunable.R:48-53`, and `extract_parameter_set_dials.model_spec()` (`/Users/max/github/parsnip/R/extract.R:86-114`) inner-joins against those 0 rows. Contrast `decision_tree`, where parsnip itself registers the identical rpart mappings (`/Users/max/github/parsnip/R/decision_tree_data.R:14-39`).

Two registry properties matter for the fix (both verified in `/Users/max/github/parsnip/R/aaa_models.R`):

- `set_model_arg()` (lines 728-768) requires only that the *model* exists (`check_model_exists()`, line 729) — not the engine. So parsnip can register args for `bag_tree`/`rpart` even though the engine's fit/predict modules stay in the extensions. Verified live: a `bag_tree` spec with engine "rpart" and mode "censored regression" can be created and `tunable()` called with no extension loaded.
- `set_model_arg()` is first-wins (lines 736-741, via the exported `model_arg_exists()`, from PR #1350): a later identical or conflicting registration of the same (engine, parsnip, original) triple is silently skipped. So baguette re-registering the same three args on top of parsnip's is a no-op, not an error.
- The `_args` table has no mode column (lines 612-621), which is why `class_cost` cannot be cleanly handled: it is classification-only, but any registration of it applies to every mode of the engine. That is issue #1351's territory.

## Proposed fix

Sequenced, with the parsnip step self-sufficient:

1. Parsnip: add a `R/bag_tree_data.R` (matching the `*_data.R` convention; the existing `R/bag_tree_rpart.R` is a doc-only file) with three `set_model_arg()` calls for the `rpart` engine, mirroring baguette's originals and `decision_tree`'s dials mappings: `cost_complexity -> cp` with `func = list(pkg = "dials", fun = "cost_complexity")`, `tree_depth -> maxdepth` with `list(pkg = "dials", fun = "tree_depth")`, `min_n -> minsplit` with `list(pkg = "dials", fun = "min_n")`, all `has_submodel = FALSE`. Do not register engines, fit, or predict modules — those stay in censored/baguette. This alone fixes the reported symptom for any load order.
2. Baguette (coordinated PR, non-urgent): drop its duplicate registrations of those three `rpart` args, adding a versioned `parsnip (>= x.y.z)` dependency so the args do not vanish for users on an older parsnip. Until that lands, nothing breaks: parsnip loads first, baguette's re-registration is skipped by the first-wins guard (and on pre-#1350 parsnip, identical rows were deduplicated by `vctrs::vec_unique()`). If maintainers instead resolve #1351 with replace/error semantics, revisit before releasing both.
3. `class_cost` stays in baguette: it is classification-only and parsnip has no mode-specific argument mechanism. Known wart to document in the PR: because args are engine-scoped, once baguette is attached, a censored-regression spec with `class_cost = tune()` will surface that parameter even though it is meaningless for survival models — pre-existing behavior, tracked by #1351, not made worse by this change.

Alternative considered in the thread: register the three args in censored as well ("in addition to baguette"). hfrick immediately preferred parsnip ("we should probably just register them in parsnip"), and the censored-side option would leave the duplicate-owner problem in place and add a third copy; only fall back to it if maintainers decide parsnip must not carry arg registrations for extension-owned engines.

Open questions to settle with maintainers before merging:

- Precedent: this would be the first place parsnip registers `set_model_arg()` for an engine it does not itself register (no `set_model_engine("bag_tree", ..., "rpart")` in parsnip). It works (verified above), but confirm the team is comfortable with args-without-engine registrations, or whether parsnip should also take over the mode-independent `set_model_engine()`/`set_dependency()` calls (a much bigger cross-repo move).
- Scope: also register baguette's `C5.0` mapping (`min_n -> minCases`) in parsnip for symmetry? No user-visible bug today (C5.0 `bag_tree` exists only in baguette, classification-only), so recommend rpart-only per the issue, noting C5.0 as optional follow-up.
- Whether the same split-ownership audit should cover the other model types censored and baguette both touch (e.g. `decision_tree`/`partykit` is registered by censored and bonsai) — candidate for a separate issue.

## Tests

Put tests in `/Users/max/github/parsnip/tests/testthat/test-bag_tree.R` (currently a placeholder that defers to baguette; keep the deferral comment for fit/predict behavior, which still lives in the extensions):

- Registry: `expect_true(model_arg_exists("bag_tree", "rpart", "cost_complexity", "cp"))` and likewise for `tree_depth`/`maxdepth` and `min_n`/`minsplit`; check the stored `func` entries with `expect_identical(..., list(pkg = "dials", fun = "cost_complexity"))` etc. (or read `get_from_env("bag_tree_args")` and use `expect_named()` on its columns).
- Tunable without extensions (this runs fine in parsnip's own suite — verified live that the spec can be built and `tunable()` called with nothing but parsnip loaded): build `bag_tree() |> set_engine("rpart") |> set_mode("censored regression")`, then on `generics::tunable(spec)` use `expect_s3_class(tbl, "tbl_df")` and `expect_identical(tbl$name, c("cost_complexity", "tree_depth", "min_n"))`; repeat for a classification-mode spec to show the args are mode-independent.
- No error/warning paths are added, so no new `expect_snapshot()` cases; end-to-end `extract_parameter_set_dials()` coverage with censored belongs in extratests (cross-ref tidymodels/extratests#186, whose baguette-loading workaround can be removed once this ships).

## Follow-ups / risks

- Draft NEWS.md bullet: `* bag_tree() now registers its cost_complexity, tree_depth, and min_n arguments for the "rpart" engine in parsnip itself, so these parameters are tunable (e.g. for censored regression via the censored package) without also loading baguette (#1398).`
- Cross-repo coordination: baguette PR to drop the duplicated registrations (with a versioned parsnip dependency); extratests PR to remove the `library(baguette)` workaround from the censored case-weights tests (tidymodels/extratests#186); censored needs no change.
- If baguette keeps re-registering in the interim, behavior depends on the duplicate-registration semantics in parsnip: current first-wins (PR #1350) makes it a silent no-op, but any future resolution of #1351 (e.g. erroring on conflicting owners) must keep identical re-registration benign, and #1260's idempotent-registration work assumes the same. These three issues should be resolved compatibly.
- `class_cost` remains mode-leaky (surfaced for censored regression once baguette is loaded) until #1351 adds mode-specific argument registration; this plan intentionally does not attempt that design.
- Risk: none to existing baguette users — the registered mappings are byte-identical to baguette's (verified against the live registry), so classification/regression tuning is unchanged.
