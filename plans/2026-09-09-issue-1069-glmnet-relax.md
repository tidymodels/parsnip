# Issue #1069: Setting `relax = TRUE` for glmnet models fails

## Issue

- Number: #1069
- Title: Setting `relax = TRUE` for glmnet models fails
- URL: https://github.com/tidymodels/parsnip/issues/1069
- Filed: 2024-02-16
- Current labels: `bug`
- Classification: the `bug` label is correct. Upstream report:
  tidymodels/poissonreg#82. The full root-cause analysis is in the issue body
  (by hfrick); it affects every tidymodels glmnet wrapper (parsnip, poissonreg,
  censored, ...), so a fix in parsnip benefits all of them.

## Symptom

Reproduced on current `main` with `devtools::load_all()` (glmnet installed).

```r
linear_reg(penalty = 0.5, mixture = 1) |>
  set_engine("glmnet", relax = TRUE) |>
  fit(mpg ~ ., data = mtcars)
#> Error in `glmnet::glmnet()`:
#> ! Base operators are not defined for quosures. Do you need to unquote
#>   the quosure?
#>
#> # Bad: myquosure > rhs
#>
#> # Good: !!myquosure > rhs
```

The backtrace confirms the analysis in the issue: `glmnet::glmnet()` calls
`glmnet::relax.glmnet()`, which calls `stats::update.default()`, which
re-`eval()`s the recorded call, and `rlang:::Ops.quosure(alpha, 1)` aborts.

With more engine arguments the failure surfaces differently
(`'language' object cannot be coerced to type 'integer'`, per the issue's
second reprex with `maxit = 1000`), but the mechanism is the same.

Inspecting the constructed fit call shows the quosures directly:

```r
spec <- linear_reg(penalty = 0.5, mixture = 1) |> set_engine("glmnet", relax = TRUE)
translate(spec)
#> Model fit template:
#> glmnet::glmnet(x = missing_arg(), y = missing_arg(), weights = missing_arg(),
#>     alpha = 1, relax = TRUE, family = "gaussian")
# but the actual args are quosures, not values:
str(translate(spec)$method$fit$args)
#> $ alpha  : language ~1
#> $ relax  : language ~TRUE
#> $ family : chr "gaussian"
```

## Root cause

- Model arguments and engine arguments are stored as quosures (`enquo()` in
  `linear_reg()` at `R/linear_reg.R:43-46`, `enquos()` in `set_engine()`).

- `translate.default()` (`R/translate.R:106`) copies these quosures unchanged
  into `x$method$fit$args`.

- At fit time, `make_form_call()`/`make_xy_call()` (`R/arguments.R:219-272`
  and `R/arguments.R:275-315`) splice the quosures into the call via
  `make_call()` (`R/arguments.R:198-216`, `call2(fun, !!!args)`), producing
  `glmnet::glmnet(..., alpha = ~1, relax = ~TRUE, ...)`.

- `eval_mod()` (`R/fit.R:381`) evaluates this call with `rlang::eval_tidy()`,
  which handles quosures fine, so ordinary fits succeed. But glmnet records its
  own `match.call()` — with the quosures as argument expressions. When
  `relax = TRUE`, `glmnet::glmnet()` internally calls `relax.glmnet()`, which
  refits the model without regularization via `stats::update.default()`, i.e.
  plain `base::eval()` of that recorded call. Quosures do not survive base
  `eval()`: `alpha` evaluates to a formula-like quosure, and glmnet's
  `alpha > 1` check hits `Ops.quosure()`, which aborts.

- Only `penalty` escapes this today: `translate.linear_reg()`
  (`R/linear_reg.R:70`) eagerly evaluates it. `mixture` (mapped to `alpha`)
  and all engine args stay lazy.

- `repair_call()` (`R/repair_call.R:30-57`) fixes exactly this quosure problem,
  but only *after* fitting — too late here, since `relax.glmnet()` runs inside
  `glmnet::glmnet()` during the fit itself.

## Proposed fix

Targeted fix for glmnet: evaluate quosures in the fit args during the glmnet
branch of `translate()`, so that the call parsnip builds — and hence the call
glmnet records and re-evaluates — contains plain values.

All parsnip glmnet translate methods already funnel through
`set_glmnet_penalty_path()` in `R/glmnet-engines.R:451-465`
(`translate.linear_reg()` at `R/linear_reg.R:67`, which `multinom_reg` reuses
via `translate.multinom_reg <- translate.linear_reg` at `R/multinom_reg.R:73`;
`translate.logistic_reg()` at `R/logistic_reg.R:83`; `translate.poisson_reg()`
at `R/poisson_reg.R:96`; `translate.proportional_hazards()` at
`R/proportional_hazards.R:94`). Add a small helper in `R/glmnet-engines.R` and
call it from `set_glmnet_penalty_path()` (or immediately after it at each call
site):

```r
x$method$fit$args <- purrr::map(
  x$method$fit$args,
  \(arg) if (rlang::is_quosure(arg)) maybe_eval(arg) else arg
)
```

Key details:

- Gate on `rlang::is_quosure()` so the protected data placeholders
  (`expr(missing_arg())` for `x`/`y`/`weights`, set in `R/translate.R:103-104`)
  and other language objects are untouched.

- Reuse the existing `maybe_eval()` helper (`R/arguments.R:151-158`) rather
  than a bare `eval_tidy()`, so quosures containing descriptors (`.obs()`,
  etc.) or otherwise unevaluable expressions are left as-is instead of
  erroring at translate time. (A descriptor in a glmnet engine arg would still
  break `relax = TRUE`, but that combination is rare and currently broken
  anyway.)

- This also covers `path_values`, which `set_glmnet_penalty_path()` moves into
  `x$method$fit$args$lambda` while it is still a quosure.

Alternatives discussed in triage (no maintainer disagreement in the thread —
the issue has no comments beyond the body):

- Evaluate quosures for *all* engines in `make_form_call()`/`make_xy_call()`
  before `make_call()`. This would also fix #432 (earth's `cv` pruning path,
  the same disease: earth re-evaluates its recorded call) but changes recorded
  calls for every engine, with a much larger blast radius for downstream
  packages and snapshots. Prefer the targeted glmnet fix now and fold the
  general version into the #878 work (removal of `eval_args()` and the
  glmnet predict wrappers), which is already rethinking quosure handling.

- A `repair_call()`-style cleanup before fitting is not viable: the failure
  happens inside `glmnet::glmnet()` itself, before parsnip ever sees a fitted
  object, so the call must be quosure-free when it is first evaluated.

Because `translate()` runs inside `fit()` for parsnip and its extension
packages, fixing this in parsnip repairs `relax = TRUE` for poissonreg
(poissonreg#82), censored's `proportional_hazards(engine = "glmnet")`, and any
other wrapper that uses parsnip's glmnet translate methods.

## Tests

Source file is `R/glmnet-engines.R`, so tests go in a new
`tests/testthat/test-glmnet-engines.R`. glmnet is not in `Suggests`, so use
`skip_if_not_installed("glmnet")` (precedent:
`tests/testthat/test-linear_reg.R:377`).

- Fit `linear_reg(penalty = 0.5, mixture = 1) |> set_engine("glmnet",
  relax = TRUE) |> fit(mpg ~ ., mtcars)` and check the result with
  `expect_s3_class(fit, "model_fit")` plus a check that `fit$fit$relaxed` is
  non-NULL (glmnet stores the relaxed fit there).

- The second reprex variant with an extra engine arg
  (`relax = TRUE, maxit = 1e5`) to cover the `'language' object` failure mode.

- A translate-level test that no element of
  `translate(spec)$method$fit$args` is a quosure for a glmnet spec with main
  and engine args; use `expect_all_false(purrr::map_lgl(args,
  rlang::is_quosure))`.

- Existing translate snapshots print quosures in glmnet fit args (e.g.
  `tests/testthat/_snaps/translate.md:595-598` shows `$nlambda <quosure>`);
  these snapshots will change to plain values and need review/accepting.

Also add or mirror a `relax = TRUE` fit test in tidymodels/extratests, where
glmnet fits run on CI unconditionally.

## Follow-ups / risks

Draft NEWS.md bullet:

- `fit()` no longer fails for glmnet engine models with `set_engine("glmnet", relax = TRUE)`; arguments are no longer left as quosures in the recorded glmnet call (#1069).

Risks and related work:

- User-visible change: `translate()` output and the fitted `$fit$call` for
  glmnet models now show literal values (`alpha = 1`) instead of quosures
  (`alpha = ~1`). Snapshot updates are needed in parsnip and possibly in
  downstream packages (poissonreg, censored, workflows) that snapshot glmnet
  templates or calls.

- `repair_call()` becomes mostly a no-op for glmnet fits (no quosures left to
  evaluate); its documentation example uses `lm`, so it is unaffected.

- Eager evaluation materializes engine-arg values at translate time. For
  glmnet these are small scalars/vectors, so no memory concern; `tune()`
  placeholders evaluate to the `tune()` call and are replaced at finalization
  before a real fit, as today.

- Quosures containing descriptors are deliberately left unevaluated by
  `maybe_eval()`; `relax = TRUE` combined with a descriptor-based engine arg
  would still fail. Not a regression.

- Related issues: #432 (earth `cv` path, same root cause; a general fix in
  `make_call()` would cover it), #878 (planned removal of `eval_args()` and
  the glmnet predict wrappers; the general quosure-eval belongs in that
  refactor), poissonreg#82 (upstream report, fixed by this change).

- No blockers; the fix is self-contained in parsnip.
