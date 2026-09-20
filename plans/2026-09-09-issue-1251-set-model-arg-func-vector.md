# Issue #1251: set_model_arg() errors uninformatively when func isn't a list

## Issue

- Number: #1251
- Title: `set_model_arg()` errors uninformatively when `func` isn't a list
- URL: https://github.com/tidymodels/parsnip/issues/1251
- Filed: 2025-02-10
- Current labels: `feature`, `documentation`
- Classification: the labels undersell it. This is a validation/documentation bug, not just a feature request. The roxygen documentation for `func` explicitly shows the failing form (`c(pkg = "stats", fun = "lm")`, a named character vector), `check_func_val()` accepts that form without complaint, and the failure only surfaces much later inside `extract_parameter_set_dials()`/`tune_grid()` with the unrelated-looking message `$ operator is invalid for atomic vectors`. The author initially suspected #1252 (an unrelated glmnet S3 dispatch issue) before isolating the real cause. EmilHvitfeldt confirmed: "Good find! That should be an easy fix on our side."

## Symptom

Reproduced live against the current checkout (registering a toy model whose tunable argument uses the documented character-vector form of `func`):

```r
devtools::load_all("/Users/max/github/parsnip")
set_new_model("my_reg")
set_model_mode("my_reg", "regression")
set_model_engine("my_reg", "regression", "glmnet")
set_model_arg(
  model = "my_reg", eng = "glmnet", parsnip = "mixture", original = "alpha",
  func = c(pkg = "dials", fun = "mixture"),   # <- documented form; passes validation
  has_submodel = FALSE
)
my_reg <- function(mode = "regression", mixture = NULL) {
  args <- list(mixture = rlang::enquo(mixture))
  new_model_spec("my_reg", args = args, eng_args = NULL, mode = mode,
                 method = NULL, engine = "glmnet")
}
spec <- set_engine(my_reg(mixture = hardhat::tune()), "glmnet")

str(generics::tunable(spec)$call_info)
#> List of 1
#>  $ : Named chr [1:2] "dials" "mixture"
#>   ..- attr(*, "names")= chr [1:2] "pkg" "fun"

hardhat::extract_parameter_set_dials(spec)
#> Error in mutate(dplyr::inner_join(dplyr::select(tuning_param, -tunable,  :
#>   i In argument: `object = purrr::map(call_info, eval_call_info)`.
#> Caused by error in `purrr::map()`:
#> i In index: 1.
#> Caused by error in `x$fun`:
#> ! $ operator is invalid for atomic vectors
```

The backtrace shows the failure is now inside parsnip itself: `extract_parameter_set_dials.model_spec()` at `R/extract.R:98` mapping `eval_call_info()` (which does `x$fun` at `R/extract.R:126,130`). The original report hit the identical copy of `eval_call_info()` in the tune package.

## Root cause

The registration path accepts a named character vector but every tunable-parameter consumer requires a list:

- `/Users/max/github/parsnip/R/aaa_models.R:728-768` — `set_model_arg()` validates `func` with `check_func_val(func)` (line 733) and stores it verbatim in the `_args` tibble's list-column (`func = list(func)`, line 751).
- `/Users/max/github/parsnip/R/aaa_models.R:326-372` — `check_func_val()` gates on `!is.vector(func)` (line 332). A named character vector *is* a vector, and all subsequent element checks use `[[` indexing (`func[["fun"]]`, `func[["pkg"]]`), which works for atomic vectors. So the documented form passes validation cleanly.
- `/Users/max/github/parsnip/R/aaa_models.R:509-515` — the shared roxygen `@param func` actively tells users to pass "A named character vector ... For example, `c(pkg = "stats", fun = "lm")`".
- `/Users/max/github/parsnip/R/tunable.R:24-27` — `tunable.model_spec()` renames the stored `func` column to `call_info` and returns it as-is.
- `/Users/max/github/parsnip/R/extract.R:116-136` — `eval_call_info()` accesses `x$fun` and `x$pkg` with `$` (line 126), which errors on atomic vectors. The tune package carries its own copy of `eval_call_info()` with the same `$` access, which is where the original traceback landed.

Why the documentation says "character vector" in the first place: for `set_fit()`/`set_pred()`, the `func` element of the fit/pred module is consumed with bracket indexing (`object$method$fit$func["fun"]` at `/Users/max/github/parsnip/R/arguments.R:267-268,309-310`; `x$func["fun"]`/`x$func["pkg"]` at `/Users/max/github/parsnip/R/predict.R:471-475`; `as.list(object$method$fit$func)` at `/Users/max/github/parsnip/R/misc.R:275`), so character vectors work fine there — indeed every fit/pred registration in parsnip uses `c(pkg = , fun = )`. Only `set_model_arg()`'s `func` flows into `$`-based consumers. All of parsnip's own `set_model_arg()` calls use the `list(pkg = , fun = )` form (e.g., `/Users/max/github/parsnip/R/decision_tree_data.R:14-39`), which is why parsnip's own registrations never trip this.

## Proposed fix

Option (a), coercion in `set_model_arg()` — recommended, and consistent with EmilHvitfeldt's "easy fix on our side":

- In `/Users/max/github/parsnip/R/aaa_models.R`, `set_model_arg()`, immediately after `check_func_val(func)` (line 733), add `func <- as.list(func)`. `as.list()` is a no-op for lists and converts a named character vector to a named list of strings, so both forms end up stored identically. This also normalizes the `vctrs::vec_unique()` dedup at line 765 (a list-form and vector-form registration of the same function would otherwise be treated as distinct rows) and fixes tune's copy of `eval_call_info()` for free, since the fix is at storage time.
- Do NOT add the coercion to `set_fit()`/`set_pred()` (their `check_fit_info()`/`check_pred_info()` calls to `check_func_val()` at lines 412 and 454 stay as-is). Their consumers use `func["fun"]` single-bracket indexing; coercing those to lists would make `func["fun"]` return a length-1 list and break `rlang::call2()`.
- Documentation: update the shared `@param func` roxygen at `/Users/max/github/parsnip/R/aaa_models.R:509-515` to say a named character vector or named list is accepted, that `fun` is required and `pkg` recommended, and that for `set_model_arg()` the value is stored as a list (with optional `range`, `trans`, and `values` elements, matching the wording already used in `check_func_val()`'s error message). Then re-document (`devtools::document()`).

Optional companion (b): make `check_func_val()` message clearer about the list form. Not required once (a) is in — no maintainer in the thread pushed for rejection over coercion, and rejecting character vectors would contradict the long-standing documentation and break `set_fit()`/`set_pred()` callers if applied globally.

Option (c), doc-only, is not sufficient: the docs currently recommend the broken form, and third-party registrations in the wild already use it.

## Tests

Add to `/Users/max/github/parsnip/tests/testthat/test-registration.R`, next to the existing `'adding a new argument'` test (line 143), which already exercises the `"sponge"`/`"gum"` toy registration:

- New test: `set_model_arg()` with `func = c(pkg = "foo", fun = "baz")` (new `parsnip`/`original` names so the first-wins guard does not skip it); read back `get_from_env("sponge_args")`, and on the stored element use `expect_type(stored, "list")`, `expect_named(stored, c("pkg", "fun"))`, and `expect_identical(stored, list(pkg = "foo", fun = "baz"))`.
- New test: registering the same argument once as a list and once as the equivalent character vector leaves a single row (guards the `vec_unique()`/first-wins interaction).
- The existing snapshot test at line 210-220 (`func = "foo::bar"`, unnamed string) must keep failing via `expect_snapshot(error = TRUE, ...)`; verify no snapshot changes.
- If any new validation message is added to `check_func_val()`, cover it with `expect_snapshot(error = TRUE, ...)`.

## Follow-ups / risks

- Draft NEWS.md bullet: `* set_model_arg() now converts a named character vector passed to func (e.g. c(pkg = "dials", fun = "mixture"), the form shown in the documentation) to a named list, so tuning no longer fails later with "$ operator is invalid for atomic vectors" (#1251).`
- Behavior change: the registry stores a list where a character vector was previously stored. Anything reading registered arg `func` values back with `[["fun"]]`/`["fun"]` still works (`/Users/max/github/parsnip/R/translate.R:148-163` just subsets the list-column); only code depending on the value being atomic would notice, which nothing in parsnip or tune does.
- Coercion happens at registration time, so extension packages are fixed automatically once they run against the new parsnip — no coordinated releases needed.
- Edge case: a character vector cannot faithfully carry the optional `range`/`trans`/`values` elements (they are typically non-character); coercing such a vector produces string elements. That input was already broken before this fix, so no regression.
- tune's private copy of `eval_call_info()` still uses `$`; storing lists makes that moot, but a hardening PR to tune (accept atomic `call_info` or error clearly) could be filed separately.
- Related but distinct: #1252 (glmnet predict dispatch), initially suspected by the same reporter; no action needed here.
