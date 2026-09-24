# Issue #1439: specific advice when `offset` is passed to `fit()`

## Overview

Since #492, `fit()` and `fit_xy()` error on anything passed through `...`. The generic message tells users to pass the argument to `set_engine()` instead. For `offset` that advice does not work, so the message now says what does.

- Issue: [#1439](https://github.com/tidymodels/parsnip/issues/1439)
- Branch: `offset-error`, off `main` at 27178260

## Work items

- [x] Establish which offset routes actually work, per interface
- [x] Add an `offset`-specific bullet to `check_fit_dots()`
- [x] Make the advice interface-aware, since `fit_xy()` has no formula
- [x] Tests in `tests/testthat/test-fit_interfaces.R`, asserting both recommended routes really apply the offset
- [x] Amend the existing `NEWS.md` bullet for #492
- [x] `air format .` and full `R CMD check`

## Why the generic advice fails

Passing a bare column name to `set_engine()` never gets evaluated against the data:

```r
linear_reg() |> set_engine("lm", offset = am) |> fit(mpg ~ wt + cyl, data = mtcars)
#> Error: invalid type (language) for variable '(offset)'
```

So a user who follows the original message lands on a second, more cryptic error. It only works if the vector is fully qualified, `offset = mtcars$am`.

## What was verified before writing the message

The original message's flaw was recommending something untested, so every route in the new message was checked first:

| Route | Works | Offset actually applied |
|---|---|---|
| `fit(y ~ x + offset(z), data = d)` | yes | yes |
| `set_engine("lm", offset = d$z)` then `fit()` | yes | yes |
| `set_engine("lm", offset = d$z)` then `fit_xy()` | yes | yes |
| `set_engine("lm", offset = z)` (bare symbol) | no | — |
| `fit(..., offset = z)` | no, errors since #492 | — |

The formula route was also confirmed for `linear_reg()` with `"glm"`, `logistic_reg()` with `"glm"`, and — less obviously — glmnet, even though parsnip converts the formula to x/y for that engine.

## The wrinkle the issue missed

`check_fit_dots()` is called from both `fit.model_spec()` and `fit_xy.model_spec()`. There is no formula in the `fit_xy()` case, so a single "use `offset()` in the formula" message would have repeated the original mistake in the other direction — recommending something impossible.

The helper therefore gained a `formula` argument, defaulting to `TRUE`, which `fit_xy.model_spec()` passes as `FALSE`. Each interface gets advice that works for it:

```
fit():     i To use an offset, include it in the formula, as in `y ~ x + offset(z)`.
fit_xy():  i To use an offset with `fit_xy()`, pass the offset vector itself to
             `set_engine()`, as in `set_engine("lm", offset = data$z)`.
```

Arguments other than `offset` are unaffected and still get only the generic message.

## NEWS

No new bullet. #492 is unreleased and already has one in this same development cycle, so it was amended rather than followed by a second bullet describing a fix to something users never saw.

## Origin

Found through parsnip's reverse-dependency check, which reported one new problem: tidypredict's `lm.Rmd` vignette calling `fit(..., offset = am)`. That offset had never been applied — before #492 the dots were silently dropped on the formula-to-formula path, and the coefficients are byte-identical to `lm(mpg ~ wt + cyl)` with no offset. See [the descriptor removal plan](2026-09-22-1645-remove-data-descriptors.md) for the full revdep results.
