# Issue #492: Disable `...` for `fit()`

## Issue

- Number: #492
- Title: Disable `...` for `fit()`
- URL: https://github.com/tidymodels/parsnip/issues/492
- Filed: 2021-05-14 (no comments on the thread)
- Current labels: `feature`
- Classification: partly a bug, not purely a feature request. The documentation for `fit()`/`fit_xy()` says `...` is "Not currently used; values passed here will be ignored" (`/Users/max/github/parsnip/R/fit.R:22-24`), but on the formula-spec/xy-engine path the dots are actually *applied* (a `subset` argument silently changes which rows are fit), and on the two `fit_xy()` paths they produce a raw, unhelpful `eval_tidy()` error. Silent behavior that contradicts the documentation is a bug; the requested informative error is the feature part.

## Symptom

All four interface combinations behave differently. Reproduced live against the current checkout (mtcars, 32 rows, `subset = 1:7`):

```r
devtools::load_all("/Users/max/github/parsnip")

# Path 1, formula spec -> formula engine (form_form): silently ignored
f1 <- linear_reg() |> set_engine("lm") |>
  fit(mpg ~ disp + hp, data = mtcars, subset = 1:7)
length(fitted(f1$fit))
#> [1] 32           # subset dropped; model fit on all rows

# Path 2, formula spec -> xy engine (form_xy): subset silently APPLIED
f2 <- linear_reg(penalty = 0) |> set_engine("glmnet") |>
  fit(mpg ~ disp + hp, data = mtcars, subset = 1:7)
f2$fit$nobs
#> [1] 7            # model fit on 7 rows, contradicting the docs

# Path 2 with a disallowed name: informative error from check_form_dots()
linear_reg(penalty = 0) |> set_engine("glmnet") |>
  fit(mpg ~ disp + hp, data = mtcars, foo = 1)
#> Error in .convert_form_to_xy_fit(formula = env$formula, data = env$data,  :
#>   The argument `foo` cannot be used to create the data.
#> Possible arguments are subset or weights.

# Path 3, xy spec -> xy engine (xy_xy): raw eval_tidy error
linear_reg(penalty = 0) |> set_engine("glmnet") |>
  fit_xy(x = mtcars[, c("disp", "hp")], y = mtcars$mpg, subset = 1:7)
#> Error in eval_tidy(e, env = envir, ...) : unused argument (subset = 1:7)

# Path 4, xy spec -> formula engine (xy_form): raw eval_tidy error
linear_reg() |> set_engine("lm") |>
  fit_xy(x = mtcars[, c("disp", "hp")], y = mtcars$mpg, subset = 1:7)
#> Error in eval_tidy(e, env = envir, ...) : unused argument (subset = 1:7)
```

Note one difference from the 2021 issue text: `check_form_dots()` now allows only `subset` and `weights` (the issue mentions `contrasts` and `offset` as well), and the formula-to-formula path drops the dots at the `fit()` switch rather than inside `eval_mod()`.

## Root cause

There is no dots check at the entry points, so each interface pathway does something different with `...`:

- `/Users/max/github/parsnip/R/fit.R:111-239` (`fit.model_spec()`): the `formula_formula` switch branch (lines 211-215) calls `form_form()` *without* forwarding `...`, so extra arguments are silently dropped (path 1). The `formula_matrix`/`formula_data.frame` branches (lines 218-231) forward `...` to `form_xy()`.
- `/Users/max/github/parsnip/R/fit_helpers.R:141-190` (`form_xy()`): forwards `...` into `.convert_form_to_xy_fit()` (lines 163-171), where `check_form_dots()` (`/Users/max/github/parsnip/R/convert_data.R:346-360`) allows `subset`/`weights` and then splices them into the `model.frame()` call (`/Users/max/github/parsnip/R/convert_data.R:76-80`) — so `subset` really subsets the training data (path 2), and anything else errors informatively.
- `/Users/max/github/parsnip/R/fit.R:246-364` (`fit_xy.model_spec()`): all switch branches (lines 332-357) forward `...` to `xy_xy()`/`xy_form()`. `xy_xy()` passes them to `eval_mod()` (`/Users/max/github/parsnip/R/fit_helpers.R:117-123`), which forwards to `rlang::eval_tidy(e, env = envir, ...)` (`/Users/max/github/parsnip/R/fit.R:368-385`); `eval_tidy()` has no matching formal, hence the raw "unused argument" dump (path 3). `xy_form()` forwards `...` to `form_form()` (`/Users/max/github/parsnip/R/fit_helpers.R:215-220`), whose own `eval_mod()` call (lines 58-64) fails the same way (path 4).

So the same user mistake is silently ignored, silently honored, or raised as an internal error depending on which of the four interface combinations is hit.

## Proposed fix

Error informatively on any non-empty `...` in both entry points, before interface dispatch, as the issue requests ("`fit()` should error informatively if the dots are used"). Concretely:

- Add a small internal helper next to `eval_mod()` in `/Users/max/github/parsnip/R/fit.R` (or in `R/arguments.R`), modeled on `check_form_dots()`:

```r
check_fit_dots <- function(..., call = rlang::caller_env()) {
  dot_names <- ...names()
  if (length(dot_names) > 0) {
    cli::cli_abort(
      c(
        "{.arg ...} must be empty.",
        "x" = "Problematic argument{?s}: {.arg {dot_names}}.",
        "i" = "Extra arguments used to fit the model should be passed to
               {.fn set_engine} instead."
      ),
      call = call
    )
  }
  invisible(NULL)
}
```

- Call `check_fit_dots(...)` in `fit.model_spec()` *after* the existing `x`/`y`-in-dots check at `/Users/max/github/parsnip/R/fit.R:158-162` (so `fit(spec, x = ..., y = ...)` keeps its specific "use `fit_xy()`" message), and near the top of `fit_xy.model_spec()`.
- The historically-sort-of-working `subset`/`weights` pass-through on the formula-to-xy path is removed: `subset` was never documented (the docs promise the opposite) and case weights now have the first-class `case_weights` argument, so there is no supported use left. Users get an error pointing at the right mechanism instead of a silently different fit.
- Clean up the now-dead forwarding: drop `...` from the `form_xy()`, `xy_xy()`, and `xy_form()` calls in the two switches, from the `form_form()` call inside `xy_form()`, and from `eval_mod()`'s signature/`eval_tidy()` calls. Keep `...` in the S3 method signatures (required by the `generics::fit()`/`fit_xy()` generics) and keep `.convert_form_to_xy_fit()`'s dots plus `check_form_dots()` unchanged — that function is an exported developer helper with its own documented `...` contract.
- Update the `@param ...` roxygen text at `/Users/max/github/parsnip/R/fit.R:22-24` ("Not currently used; values passed here will be ignored") to say the dots must be empty and that an error is raised otherwise, then re-document (`man/fit.Rd`).

No maintainer disagreement exists in the thread, so no alternative is required; a softer option (one release of `lifecycle::deprecate_warn()` for `subset`/`weights` on the formula-to-xy path only, since that combination demonstrably worked since at least 2021) is listed under follow-ups as a decision point.

## Tests

Add to `/Users/max/github/parsnip/tests/testthat/test-fit_interfaces.R` (snapshots in `tests/testthat/_snaps/fit_interfaces.md`). Because the check runs before engine dispatch, `lm` alone covers both entry points and no Suggests packages are needed:

- `expect_snapshot(error = TRUE, linear_reg() |> fit(mpg ~ disp + hp, data = mtcars, subset = 1:7))` — formula entry.
- `expect_snapshot(error = TRUE, linear_reg() |> fit_xy(x = mtcars[, c("disp", "hp")], y = mtcars$mpg, subset = 1:7))` — xy entry.
- One case with multiple dots (e.g. `subset = 1:7, foo = 1`) to snapshot the pluralized message.
- Keep/verify the existing `fit(spec, x = ..., y = ...)` "use `fit_xy()`" snapshot still wins over the new check (ordering test).
- If any existing tests in `test-fit_interfaces.R` or `test-convert_data.R` rely on `fit(..., subset = )` or `fit(..., weights = )` passing through, update them to call `.convert_form_to_xy_fit()` directly (its behavior is unchanged).

## Follow-ups / risks

- Draft NEWS.md bullet: `fit()` and `fit_xy()` now error informatively when extra arguments are passed through `...`; previously such arguments were silently ignored, silently applied, or raised an unhelpful internal error depending on the combination of user and engine interfaces (#492).
- Behavior tightening: code that passed `subset` or `weights` to `fit()` with an xy-interface engine (e.g. glmnet) got a genuinely subsetted/weighted fit before and will now error. This warrants a revdepcheck run and possibly a blog/NEWS callout; extension packages (censored, bonsai, etc.) and tune/workflows do not pass dots into `fit()`/`fit_xy()` themselves, but user code may.
- Decision point for maintainers: hard error immediately (proposed) versus one release of a deprecation warning for the previously functional `subset`/`weights` on the formula-to-xy path.
- Related: the `...` docs for `fit()`/`fit_xy()` and the `case_weights` argument should cross-reference each other so ex-`weights` users find the replacement.
- No blockers; the change is local to `R/fit.R` and `R/fit_helpers.R`.
