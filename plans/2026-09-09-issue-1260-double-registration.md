# Issue #1260: "Model already registered" error prevents attaching packages with custom models to fit_resamples and tune_grid

## Issue

- Number: #1260
- Title: Parsnip's "Model already registered" error prevents attaching packages with custom models to fit_resamples and tune_grid
- URL: https://github.com/tidymodels/parsnip/issues/1260
- Filed: 2025-02-20
- Current labels: `bug`, `documentation`
- Classification: the `bug` label fits the parsnip half of the report. The report actually contains two problems: (1) custom yardstick metrics are not available on parallel workers unless the extension package is loaded there — that is a tune/parallelism issue, explicitly acknowledged by topepo as "an overall issue that we are trying to solve", and out of scope for parsnip; (2) listing the extension package in `control_resamples(pkgs = )` fails because the package's `.onLoad()` re-runs its model registration and `set_new_model()` hard-aborts with "Model ... already exists". Only (2) is actionable in parsnip.
- Status flag: topepo asked for a reproducible example of the worker-side registry state ("When you start a multisession plan, the worker processes are empty so I'm not sure why parsnip would have it") and the reporter never supplied one. The issue has been silent since February 2025. The exact transport mechanism (how the registry entry reaches the worker before `library(my_pkg)` runs — e.g. future's globals serialization pulling in environments, tune's package-loading order on workers, or a nested/sequential plan where workers share the main process) remains unconfirmed and should be verified or at least noted in the PR. The single-process failure mode, however, is fully reproducible and worth fixing regardless: any situation where an extension's registration code runs twice against the same registry (repeated `devtools::load_all()`, `unloadNamespace()` + reload, two packages registering the same model, `pkgs =` on a worker that already has the model) hits the same abort.

## Symptom

Minimal single-process demonstration of the abort, reproduced live against the current checkout:

```r
devtools::load_all("/Users/max/github/parsnip")
set_new_model("foo")
set_new_model("foo")
#> Error in `set_new_model()`:
#> ! Model "foo" already exists.
```

Backtrace (observed):

```
 1. parsnip::set_new_model("foo")
 2.   parsnip:::check_model_doesnt_exist(model) at parsnip/R/aaa_models.R:594:3
 3.     cli::cli_abort("Model {.val {model}} already exists.", call = call) at parsnip/R/aaa_models.R:152:5
```

In the reported scenario this abort fires inside the extension package's `.onLoad()` when tune loads `pkgs = "my_pkg"` on a worker whose parsnip registry already contains the model, which kills the whole `fit_resamples()`/`tune_grid()` call. (Not reproduced live here: it needs a multi-package parallel setup, and the reporter never provided the promised reprex.)

## Root cause

The model registry is the `parsnip` environment created at build time inside the parsnip namespace (`/Users/max/github/parsnip/R/aaa_models.R:33-35`, returned by `get_model_env()` at lines 75-77). It therefore travels with the parsnip namespace, and any serialization/loading path that delivers a populated registry to a process makes a second registration attempt fatal.

The fatal piece is `set_new_model()`:

- `/Users/max/github/parsnip/R/aaa_models.R:593-641` — `set_new_model()` calls `check_model_doesnt_exist(model)` (line 594) as its first action.
- `/Users/max/github/parsnip/R/aaa_models.R:146-159` — `check_model_doesnt_exist()` aborts with `Model {.val {model}} already exists.` whenever the model name is present in `current$models`.
- `check_model_doesnt_exist()` is internal (not in `NAMESPACE`), so extension authors cannot even guard their `.onLoad()` without `:::` — the reporter noted exactly this and fell back to wrapping `set_new_model()` in `try()`.

Crucially, `set_new_model()` is the only registration function left that hard-fails on re-registration. Every other `set_*()` is already idempotent for identical information (the #653 policy, "Models information can be re-registered as long as the information being registered is the same", plus the newer first-wins arg handling from PR #1350):

- `set_model_mode()` (lines 648-663): `unique()` on the mode vectors.
- `set_model_engine()` (lines 670-687): `dplyr::distinct()` on the engine/mode tibble.
- `set_dependency()` (lines 777-844): `dplyr::distinct()` plus per-engine aggregation of unique package names.
- `set_model_arg()` (lines 728-768): first-wins skip via the exported `model_arg_exists()` (lines 705-722) plus `vctrs::vec_unique()`.
- `set_fit()` (lines 934-970), `set_pred()` (lines 991-1030), and `set_encoding()` (via line 1233): no-op when identical, informative abort only when the re-registered information *differs*, via `is_discordant_info()` (lines 864-910).

`tests/testthat/test-re_registration.R` documents that idempotence for modes, engines, dependencies, fit, encoding, and prediction — but has no case for `set_new_model()` itself. So an extension's full registration script re-runs cleanly *except* for its first line.

## Proposed fix

Make `set_new_model()` idempotent, completing the #653/#1350 policy:

- In `/Users/max/github/parsnip/R/aaa_models.R`, `set_new_model()`: validate the name with `check_string(model, allow_empty = FALSE)` and, if `any(get_model_env()$models == model)`, return `invisible(NULL)` early instead of aborting. Skipping is always safe here because `set_new_model()` only creates empty scaffolding tibbles (`<model>`, `<model>_pkgs`, `<model>_modes`, `<model>_args`, `<model>_fit`, `<model>_predict`, lines 598-638); an existing registration is at least as complete, and any genuinely conflicting *content* is still caught downstream by `is_discordant_info()` when `set_fit()`/`set_pred()`/`set_encoding()` re-run.
- `check_model_doesnt_exist()` then has no remaining callers (verified: line 594 is the only call site in `R/` and `tests/`). Either keep it (it is referenced in the roxygen details at lines 545-547) or remove it and drop that sentence from the docs; keeping it as an internal helper is the smaller diff. Update the `set_new_model()` roxygen details to state that re-registering an existing model is a no-op.
- With this change, an extension package's `.onLoad()` registration script can run any number of times against the same registry, which directly unblocks `control_resamples(pkgs = "my_pkg")` for the reported failure mode, whatever the registry-transport mechanism turns out to be.

Alternative/complement (only if maintainers prefer an explicit guard over silent idempotence — nobody in the thread argued for this): export a small predicate (e.g. `model_exists(model)`), mirroring the already-exported `model_arg_exists()`, so extension authors can write `if (!model_exists("my_reg")) set_new_model("my_reg")`. Prior art in the wild: extensions wrap `set_new_model()` in `try()` or inspect `get_model_env()$models` directly. This helps but does not fix existing packages without a new release of each, so the idempotent `set_new_model()` should land either way.

Things to confirm before/while fixing (from the unanswered reprex request):

- How the worker's registry comes to contain the model before `pkgs` loading runs (future globals serialization vs. tune loading order); a two-process reprex with a toy extension package would settle it.
- That tune's worker setup loads packages via namespace loading (`.onLoad()` runs once per process) and the failure indeed comes through parsnip's registration abort surfacing as a worker error.
- Whether the metric half of the report is already tracked in tune (topepo: "we're working on how to send global data to the worker processes"); if there is a tune issue, cross-link it and scope this one to the registration abort.

## Tests

Add to `/Users/max/github/parsnip/tests/testthat/test-re_registration.R` (the file that documents the #653 idempotence policy), alongside the existing `'re-registration of mode'` test:

- `'re-registration of a model'`: call `set_new_model()` twice for a fresh toy model name; wrap the second call in `expect_no_condition()`; snapshot nothing (no message expected). Then confirm the registry is unchanged: `expect_equal(sum(get_model_env()$models == "<name>"), 1)` and compare the six scaffolding objects before/after the second call with `expect_equal()`.
- Confirm that populated state survives a re-run: register a mode/engine/arg for the toy model, call `set_new_model()` again, and check the `_args` and `<model>` tibbles are untouched.
- The invalid-input snapshots in `/Users/max/github/parsnip/tests/testthat/test-registration.R:57-59` (`set_new_model()`, `set_new_model(2)`, `set_new_model(letters[1:2])`) must keep passing — the name validation stays. There is no existing snapshot of the "already exists" error to update (verified), but grep `tests/testthat/_snaps/` for `already exists` before merging in case another suite exercises it indirectly.

## Follow-ups / risks

- Draft NEWS.md bullet: `* set_new_model() is now a no-op instead of an error when the model is already registered, so extension packages that register models in .onLoad() can be listed in the pkgs option of tune/resampling control functions and loaded repeatedly without a "Model already exists" error (#1260).`
- Behavior change: code that relied on `set_new_model()` erroring for an existing model (e.g. as an existence probe) will no longer error. No such use exists in parsnip or its tests; extension packages wrapping the call in `try()` are unaffected.
- This does not by itself put custom yardstick metrics on workers (the other half of the report); that belongs to tune's parallel-globals work and should be cross-referenced when closing.
- Related issues: #653 (original re-registration policy), tidymodels/parsnip PR #1350 and issue #1351 (duplicate/conflicting arg registration semantics across extension packages), #1398 (relies on duplicate-arg registration being benign when parsnip takes over registrations from baguette).
- Blocker/unknown: the worker-side mechanism is unverified (no reprex from the reporter). The fix is justified by the single-process reproduction alone, but the PR text should be honest that the parallel scenario was not reproduced.
