# Issue #974: one-class SVM with `kernlab` fails to produce predictions

## Issue

- Number: #974
- Title: one-class SVM with `kernlab` fails to produce predictions
- URL: https://github.com/tidymodels/parsnip/issues/974
- Filed: 2023-05-26
- Current labels: `bug`
- Classification: the label is defensible but incomplete. The crash in `predict_class.model_fit()` is a genuine bug (an unhandled return shape produces an uninformative internal error). However, the maintainer discussion makes clear that one-class SVM was never an intended, supported configuration in parsnip: simonpcouch asked whether it belongs in tidyclust, EmilHvitfeldt said it is closer to anomaly detection (see tidymodels/applicable#19), and frankiethull has since implemented one-class SVM support in his own package ("maize", based on the applicable isolation-forest implementation). So the actionable classification is closer to "unsupported configuration that fails with an unhelpful error" than "supported feature that is broken." The fix should make the failure informative, not make the feature work.

## Symptom

Reproduced live against the current checkout (kernlab installed):

```r
devtools::load_all("/Users/max/github/parsnip")
set.seed(48217)
df <- data.frame(x1 = rnorm(100), x2 = rnorm(100) + 2)
df$fake_outcome <- factor(rep("only_class", nrow(df)))

spec <- svm_rbf() |>
  set_mode("classification") |>
  set_engine("kernlab", type = "one-svc")

fit_obj <- fit(spec, fake_outcome ~ x1 + x2, data = df)
predict(fit_obj, new_data = df[1:3, ])
#> Error in res$values : $ operator is invalid for atomic vectors
```

Inspecting what kernlab returns for this fit confirms the diagnosis:

```r
raw <- kernlab::predict(fit_obj$fit, df[1:3, 1:2], type = "response")
class(raw)   # "matrix" "array"
dim(raw)     # 3 x 1
typeof(raw)  # "logical"
```

Secondary design wart, also reported in the issue: one-class novelty detection has no outcome, but the only way to fit through parsnip is to fabricate a fake single-level factor outcome column for the formula (`fake_outcome` above). There is no supported no-outcome fitting interface in parsnip, which is further evidence that this model type does not fit parsnip's API.

## Root cause

Two layers:

1. `kernlab::ksvm()` with `type = "one-svc"` is a one-class novelty-detection model. Its `predict()` method with `type = "response"` returns an `n x 1` logical matrix (in/out of class), not a factor of class predictions. Verified live above.

2. In `/Users/max/github/parsnip/R/predict_class.R`, `predict_class.model_fit()` post-processes the engine result (the kernlab class-pred registration in `/Users/max/github/parsnip/R/svm_rbf_data.R` lines 120-135 has `post = NULL`, so `res` arrives untouched):

   - Lines 47-52: `if (is.vector(res) || is.factor(res))` — a matrix is neither (`is.vector()` is `FALSE` for a matrix because of the `dim` attribute), so this branch is skipped.
   - Lines 53-64: the else-branch assumes `res` is a Spark table or a data frame. A logical matrix fails the `is.data.frame(...)` test at line 56, falls through to line 59, and `res$values <- factor(...)` errors with `$ operator is invalid for atomic vectors` because `$` is not defined for atomic matrices.

There is no validation anywhere (in `translate.svm_rbf()`, `/Users/max/github/parsnip/R/svm_rbf.R` lines 100-143, or at fit time) that the kernlab `type` engine argument is consistent with the parsnip mode, so the unsupported configuration sails through `fit()` and only detonates at `predict()` with an internal error.

The same applies to `svm_poly` and `svm_linear` with the kernlab engine: any of kernlab's non-classification `type` values (`"one-svc"`, `"eps-svr"`, `"nu-svr"`, `"eps-bsvr"`) can be smuggled in under `set_mode("classification")`, and vice versa.

## Proposed fix

Recommendation: option (b), explicitly unsupport `type = "one-svc"` (and other mode-inconsistent kernlab types) with an informative error at translate time. The maintainer thread shows no appetite for supporting one-class SVM in parsnip (tidyclust was floated and rejected; applicable and the third-party maize package were named as the right homes), and option (a) alone would still leave users with a fitted model whose "class" predictions are logical in/out flags that cannot be mapped onto a factor with the training levels — parsnip's classification contract fundamentally does not fit this model.

### (b) Error informatively at translate time (recommended)

There is direct precedent in `translate.svm_linear()` (`/Users/max/github/parsnip/R/svm_linear.R` lines 95-124), which already aborts when the LiblineaR `type` engine argument does not correspond to the spec's mode. Mirror that for kernlab:

- In the `x$engine == "kernlab"` block of `translate.svm_rbf()` (`/Users/max/github/parsnip/R/svm_rbf.R`, around line 108), read `x$eng_args$type` (via `quo_get_expr()`, as the LiblineaR check does) and abort with `cli::cli_abort()` when it is `"one-svc"` (or, more completely, when it is not one of kernlab's types for the current mode: classification allows `"C-svc"`, `"nu-svc"`, `"C-bsvc"`, `"spoc-svc"`, `"kbb-svc"`; regression allows `"eps-svr"`, `"nu-svr"`, `"eps-bsvr"`).
- The error for `"one-svc"` should say that one-class SVM (novelty/anomaly detection) is not supported by parsnip and point users at alternatives named by the maintainers: the applicable package (tidymodels/applicable#19) and the maize package.
- Since the kernlab blocks are duplicated across `translate.svm_rbf()`, `translate.svm_poly()` (`/Users/max/github/parsnip/R/svm_poly.R`), and `translate.svm_linear()`, put the check in a small shared internal helper (e.g. `check_kernlab_type(x)` next to the other check helpers) and call it from all three, rather than pasting the logic three times.

### (a) Defensive fix in predict_class (documented, not recommended alone)

The minimal crash fix would be to make the else-branch of `predict_class.model_fit()` (`/Users/max/github/parsnip/R/predict_class.R` lines 53-66) handle non-list results: if `res` is an atomic matrix (or otherwise not a list/data frame/Spark table), raise an informative `cli::cli_abort()` saying the engine returned an object parsnip cannot interpret as class predictions, instead of blindly doing `res$values <- ...`. This is worth doing anyway as a guard for other misbehaving engines, but it does not make one-svc usable and should not be sold as such. Do not try to coerce the logical matrix into a factor: the training data has one fake level, so the in/out flags have no honest factor mapping.

If both are implemented, (b) means users never reach (a) for this configuration; (a) just improves the failure mode for any future engine that returns an unexpected shape.

## Tests

Add to `/Users/max/github/parsnip/tests/testthat/test-svm_rbf.R` (next to the existing `'bad input'` tests at line 23):

- A test that `svm_rbf() |> set_mode("classification") |> set_engine("kernlab", type = "one-svc") |> translate()` errors, using `expect_snapshot(error = TRUE)` so the full message (including the pointers to applicable/maize) is reviewed.
- If the mode/type consistency check is generalized, also snapshot a regression-mode spec with a classification `type` (e.g. `type = "C-svc"` with `set_mode("regression")`).
- Parallel one-line snapshot tests in `tests/testthat/test-svm_poly.R` and `tests/testthat/test-svm_linear.R` if the shared helper is wired into those translate methods.
- If option (a) is also implemented, a unit test for the new guard in `tests/testthat/test-predict_class.R` (create if absent) using a minimal fake `model_fit` whose pred call returns a logical matrix, with `expect_snapshot(error = TRUE)`.

No `skip_if_not_installed("kernlab")` is needed for the translate-time tests since `translate()` does not call kernlab.

## Follow-ups / risks

- Draft NEWS.md bullet: `svm_rbf()`, `svm_poly()`, and `svm_linear()` with the kernlab engine now error informatively at `translate()` when the `type` engine argument requests an unsupported model such as one-class SVM (`type = "one-svc"`), instead of failing at `predict()` with an internal error (#974).
- Behavior change: specs with `type = "one-svc"` that previously fit (but could never predict) will now error at translate/fit time. This can only break code that fit such models and never predicted with them, which seems acceptable.
- The fake-outcome design wart is not fixable inside parsnip; fitting without an outcome is out of scope for its API. Note it in the closing comment and point to applicable/maize.
- Related: tidymodels/applicable#19 (anomaly detection scope), frankiethull's maize package (has a working one-class SVM implementation modeled on applicable's isolation-forest interface).
- Risk: hard-coding kernlab's allowed `type` values could drift if kernlab adds types; keep the check scoped to aborting on known-bad values (one-svc and cross-mode types) rather than an allowlist if that is a concern.
