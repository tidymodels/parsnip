# Issues #472 and #1409: earth multiclass class predictions use a binary threshold rule

## Issue

Two issues describe the same defect; one fix closes both.

- Number: #472
- Title: Support for multi-level outcomes in MARS classification
- URL: https://github.com/tidymodels/parsnip/issues/472
- Filed: 2021-04-20
- Current labels: `feature`, `tidy-dev-day :nerd_face:`
- Classification: the `feature` label undersells it. parsnip happily *fits* a
  multiclass earth model (juliasilge confirmed this in the thread) and then
  silently returns wrong class predictions, which is a correctness bug, not a
  missing feature.

- Number: #1409
- Title: earth multiclass classification predictions use binary threshold rule, giving wrong classes
- URL: https://github.com/tidymodels/parsnip/issues/1409
- Filed: 2026-09-01
- Current labels: `tidy-dev-day :nerd_face:` (no comments)
- Classification: should carry the `bug` label — silent wrong predictions.

The `type = "prob"` half of #472 was already fixed by PR #1334, which replaced
the binary-only prob post-processor with `earth_glm_covert()`
(/Users/max/github/parsnip/R/mars_data.R:153, helper at
/Users/max/github/parsnip/R/aaa_models.R:1286-1295). The `type = "class"`
post-processor was not touched and is still binary-only.

## Symptom

Reproduced live against the current checkout (earth installed):

```r
devtools::load_all("/Users/max/github/parsnip")
f <- mars(mode = "classification") |>
  set_engine("earth") |>
  fit(Species ~ ., data = iris)
#> (glm.fit convergence warnings for the separable setosa class)
p <- predict(f, iris, type = "class")
table(p$.pred_class, iris$Species)
#>              setosa versicolor virginica
#>   setosa          0         50        50
#>   versicolor     50          0         0
#>   virginica       0          0         0
```

Every prediction is wrong: the 50 true setosa rows are predicted
"versicolor" and the other 100 rows are predicted "setosa"; "virginica" can
never be predicted. Meanwhile `type = "prob"` is correct (fixed by #1334):

```r
predict(f, iris, type = "prob")
#> # names: .pred_setosa .pred_versicolor .pred_virginica  (correct)
```

## Root cause

The `type = "class"` post function for the earth engine at
/Users/max/github/parsnip/R/mars_data.R:126-144 is hardcoded for binary
outcomes:

```r
post = function(x, object) {
  x <- ifelse(x[, 1] >= 0.5, object$lvl[2], object$lvl[1])
  x
}
```

Verified live: for a two-class outcome, earth's
`predict(fit, type = "response")` returns a one-column matrix of the
probability of the *second* factor level (column name `Class2` for
`two_class_dat`), so the threshold rule is correct there. For a 3+ class
outcome, earth returns one column per class (column names
`setosa versicolor virginica`, matching `object$lvl` order for iris), so
`x[, 1] >= 0.5` only ever tests the first class's probability and maps every
row to one of the first two levels.

The downstream conversion at /Users/max/github/parsnip/R/predict_class.R:47-53
turns whatever character vector the post returns into a factor with
`levels = object$lvl`, so the post only needs to return correct level strings.

## Proposed fix

Branch on the number of outcome levels in the class post at
/Users/max/github/parsnip/R/mars_data.R:126-144, preferring earth's column
names when present (they match `object$lvl` order for iris, both being derived
from the factor levels, but using them directly is more robust):

```r
post = function(x, object) {
  if (length(object$lvl) == 2) {
    x <- ifelse(x[, 1] >= 0.5, object$lvl[2], object$lvl[1])
  } else {
    max_prob_col <- apply(x, 1, which.max)
    if (is.null(colnames(x))) {
      x <- object$lvl[max_prob_col]
    } else {
      x <- colnames(x)[max_prob_col]
    }
  }
  x
}
```

Verified live that this logic produces the right answers: applying
`colnames(raw)[apply(raw, 1, which.max)]` to earth's response matrix for iris
rows 1, 51, and 101 yields `setosa versicolor virginica`.

For consistency with `earth_glm_covert()`, define this as a named helper
(e.g., `earth_class_pred()`) next to it in
/Users/max/github/parsnip/R/aaa_models.R and register `post = earth_class_pred`
in mars_data.R, paralleling how PR #1334 handled the prob type.

Alternative considered: normalize through `earth_glm_covert()` and take the
max-probability column for both binary and multiclass. This unifies the two
branches but changes binary tie behavior (probability exactly 0.5 currently
predicts `lvl[2]`; `which.max` would pick `lvl[1]`), so keeping the existing
binary rule is the lower-risk choice. No maintainer disagreement exists in
either thread.

## Tests

Add to /Users/max/github/parsnip/tests/testthat/test-mars.R, next to the
existing `'classification probabilities for multiclass'` test (added for
#1334 at lines 283-317), a parallel
`test_that("class predictions for multiclass", ...)` with
`skip_if_not_installed("earth")` and `skip_if_not_installed("modeldata")`:

- Multiclass: fit `mars(mode = "classification", engine = "earth")` on iris
  (wrapped in `suppressWarnings()` like the #1334 test), predict
  `type = "class"` on rows `c(1, 51, 101)`, then
  `expect_named(res, ".pred_class")`,
  `expect_s3_class(res$.pred_class, "factor")`,
  `expect_equal(levels(res$.pred_class), levels(iris$Species))`, and
  `expect_equal(as.character(res$.pred_class), c("setosa", "versicolor", "virginica"))`.
- Binary regression guard: fit on `two_class_dat`, predict `type = "class"`,
  and check the predictions agree with thresholding
  `predict(type = "prob")$.pred_Class2` at 0.5, to pin the unchanged binary
  behavior.

## Follow-ups / risks

- Draft NEWS.md bullet: `mars()` classification fits with the earth engine now return correct `type = "class"` predictions for outcomes with three or more levels; previously a binary threshold rule silently produced wrong classes (#472, #1409).
- Closing #472 should note that the prob half was fixed earlier by PR #1334 and this change completes the class half; #1409 is a duplicate of the remaining half and closes with the same commit.
- The same hardcoded binary rule may exist downstream and should be checked in those repos (fixes belong there, not in parsnip): discrim's `discrim_flexible()` earth/fda prediction post-processors (tidymodels/discrim), and baguette's `bag_mars()` class prediction path (tidymodels/baguette). File issues there if the pattern `ifelse(x[, 1] >= 0.5, ...)` appears.
- Risk is low: binary behavior is unchanged, and multiclass class predictions could not previously have been correct, so no one can be depending on the old output.
- The multiclass glm fit itself can throw `glm.fit` convergence warnings (earth fits one binomial glm per level); that is earth behavior and out of scope here.

## Work items

Executed 2026-09-22 on branch `earth-multiclass-class-pred` (off `main` at 8ca7373e).

- [x] Reproduce the bug on `main` and confirm the shape of earth's `type = "response"` output for binary and multiclass outcomes
- [x] Add `earth_class_pred()` next to `earth_glm_covert()` in `R/aaa_models.R`
- [x] Register it as the `type = "class"` post for the earth engine in `R/mars_data.R`
- [x] Add `class predictions for multiclass` to `tests/testthat/test-mars.R`, beside the `#1334` prob test
- [x] Add the `NEWS.md` bullet
- [x] Check the downstream packages named below for the same pattern
- [x] `air format .` and full `devtools::test()`

### Notes from execution

The helper branches on `ncol(x) == 1` rather than the `length(object$lvl) == 2` the plan proposed. Both are equivalent for earth, but keying on the column count matches its sibling `earth_glm_covert()` exactly and tests the thing the code actually indexes into. Verified live: for `two_class_dat` earth returns a one-column matrix named `Class2` (the *second* level's probability, which is what makes the existing threshold rule correct), and for iris it returns a 150x3 matrix with colnames `setosa versicolor virginica` matching `object$lvl`.

The plan's decision not to unify the two branches through `earth_glm_covert()` is worth keeping: `which.max` on a normalized two-column matrix would flip the exact-0.5 tie from `lvl[2]` to `lvl[1]`. No row in `two_class_dat` sits at exactly 0.5, so it is latent rather than observable, but it would be a silent behavior change for no gain.

Before and after on iris, all 150 rows:

```
before                                  after
             set  ver  vir                          set  ver  vir
  setosa       0   50   50                setosa     50    0    0
  versicolor  50    0    0                versicolor  0   49    1
  virginica    0    0    0                virginica   0    1   49
```

The two remaining errors are the genuinely overlapping versicolor/virginica pair.

Beyond the plan's suggested assertions, the test also checks that the predicted class equals the arg-max of `predict(type = "prob")` across all 150 iris rows, and that binary predictions equal a 0.5 threshold on `.pred_Class2` across all of `two_class_dat`. Those two tie `class` to `prob` directly, which is the invariant that was actually broken.

### Downstream check

The follow-up below anticipated the same `ifelse(x[, 1] >= 0.5, ...)` rule in discrim and baguette. It is not there, so there is nothing to file:

- discrim 1.1.0: `discrim_flexible()` registers `type = "class"` with `post = NULL`, delegating to `discrim::pred_wrapper()` and on to `mda::predict.fda()`, which handles multiclass natively. Its `prob` type uses `prob_matrix_to_tibble` with `type = "posterior"`.
- baguette 1.1.0: `bag_mars()` registers `type = "class"` with `post = fix_column_names`, delegating to baguette's own bagged `predict()` with `type = "class"`.

Neither `R/` tree contains a `0.5` threshold in any prediction path.
