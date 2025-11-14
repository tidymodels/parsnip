# Augment data with predictions

[`augment()`](https://generics.r-lib.org/reference/augment.html) will
add column(s) for predictions to the given data.

## Usage

``` r
# S3 method for class 'model_fit'
augment(x, new_data, eval_time = NULL, ...)
```

## Arguments

- x:

  A [model
  fit](https://parsnip.tidymodels.org/dev/reference/model_fit.md)
  produced by
  [`fit.model_spec()`](https://parsnip.tidymodels.org/dev/reference/fit.md)
  or
  [`fit_xy.model_spec()`](https://parsnip.tidymodels.org/dev/reference/fit.md).

- new_data:

  A data frame or matrix.

- eval_time:

  For censored regression models, a vector of time points at which the
  survival probability is estimated.

- ...:

  Not currently used.

## Details

### Regression

For regression models, a `.pred` column is added. If `x` was created
using
[`fit.model_spec()`](https://parsnip.tidymodels.org/dev/reference/fit.md)
and `new_data` contains a regression outcome column, a `.resid` column
is also added.

### Classification

For classification models, the results can include a column called
`.pred_class` as well as class probability columns named
`.pred_{level}`. This depends on what type of prediction types are
available for the model.

### Censored Regression

For these models, predictions for the expected time and survival
probability are created (if the model engine supports them). If the
model supports survival prediction, the `eval_time` argument is
required.

If survival predictions are created and `new_data` contains a
[`survival::Surv()`](https://rdrr.io/pkg/survival/man/Surv.html) object,
additional columns are added for inverse probability of censoring
weights (IPCW) are also created (see `tidymodels.org` page in the
references below). This enables the user to compute performance metrics
in the yardstick package.

### Quantile Regression

For quantile regression models, a `.pred_quantile` column is added that
contains the quantile predictions for each row. This column has a
special class `"quantile_pred"` and can be unnested using
[`tidyr::unnest()`](https://tidyr.tidyverse.org/reference/unnest.html)

## References

<https://www.tidymodels.org/learn/statistics/survival-metrics/>

## Examples

``` r
car_trn <- mtcars[11:32,]
car_tst <- mtcars[ 1:10,]

reg_form <-
  linear_reg() |>
  set_engine("lm") |>
  fit(mpg ~ ., data = car_trn)
reg_xy <-
  linear_reg() |>
  set_engine("lm") |>
  fit_xy(car_trn[, -1], car_trn$mpg)

augment(reg_form, car_tst)
#> # A tibble: 10 × 13
#>    .pred .resid   mpg   cyl  disp    hp  drat    wt  qsec    vs    am
#>    <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1  23.4 -2.43   21       6  160    110  3.9   2.62  16.5     0     1
#>  2  23.3 -2.30   21       6  160    110  3.9   2.88  17.0     0     1
#>  3  27.6 -4.83   22.8     4  108     93  3.85  2.32  18.6     1     1
#>  4  21.5 -0.147  21.4     6  258    110  3.08  3.22  19.4     1     0
#>  5  17.6  1.13   18.7     8  360    175  3.15  3.44  17.0     0     0
#>  6  21.6 -3.48   18.1     6  225    105  2.76  3.46  20.2     1     0
#>  7  13.9  0.393  14.3     8  360    245  3.21  3.57  15.8     0     0
#>  8  21.7  2.70   24.4     4  147.    62  3.69  3.19  20       1     0
#>  9  25.6 -2.81   22.8     4  141.    95  3.92  3.15  22.9     1     0
#> 10  17.1  2.09   19.2     6  168.   123  3.92  3.44  18.3     1     0
#> # ℹ 2 more variables: gear <dbl>, carb <dbl>
augment(reg_form, car_tst[, -1])
#> # A tibble: 10 × 11
#>    .pred   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1  23.4     6  160    110  3.9   2.62  16.5     0     1     4     4
#>  2  23.3     6  160    110  3.9   2.88  17.0     0     1     4     4
#>  3  27.6     4  108     93  3.85  2.32  18.6     1     1     4     1
#>  4  21.5     6  258    110  3.08  3.22  19.4     1     0     3     1
#>  5  17.6     8  360    175  3.15  3.44  17.0     0     0     3     2
#>  6  21.6     6  225    105  2.76  3.46  20.2     1     0     3     1
#>  7  13.9     8  360    245  3.21  3.57  15.8     0     0     3     4
#>  8  21.7     4  147.    62  3.69  3.19  20       1     0     4     2
#>  9  25.6     4  141.    95  3.92  3.15  22.9     1     0     4     2
#> 10  17.1     6  168.   123  3.92  3.44  18.3     1     0     4     4

augment(reg_xy, car_tst)
#> # A tibble: 10 × 12
#>    .pred   mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear
#>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1  23.4  21       6  160    110  3.9   2.62  16.5     0     1     4
#>  2  23.3  21       6  160    110  3.9   2.88  17.0     0     1     4
#>  3  27.6  22.8     4  108     93  3.85  2.32  18.6     1     1     4
#>  4  21.5  21.4     6  258    110  3.08  3.22  19.4     1     0     3
#>  5  17.6  18.7     8  360    175  3.15  3.44  17.0     0     0     3
#>  6  21.6  18.1     6  225    105  2.76  3.46  20.2     1     0     3
#>  7  13.9  14.3     8  360    245  3.21  3.57  15.8     0     0     3
#>  8  21.7  24.4     4  147.    62  3.69  3.19  20       1     0     4
#>  9  25.6  22.8     4  141.    95  3.92  3.15  22.9     1     0     4
#> 10  17.1  19.2     6  168.   123  3.92  3.44  18.3     1     0     4
#> # ℹ 1 more variable: carb <dbl>
augment(reg_xy, car_tst[, -1])
#> # A tibble: 10 × 11
#>    .pred   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1  23.4     6  160    110  3.9   2.62  16.5     0     1     4     4
#>  2  23.3     6  160    110  3.9   2.88  17.0     0     1     4     4
#>  3  27.6     4  108     93  3.85  2.32  18.6     1     1     4     1
#>  4  21.5     6  258    110  3.08  3.22  19.4     1     0     3     1
#>  5  17.6     8  360    175  3.15  3.44  17.0     0     0     3     2
#>  6  21.6     6  225    105  2.76  3.46  20.2     1     0     3     1
#>  7  13.9     8  360    245  3.21  3.57  15.8     0     0     3     4
#>  8  21.7     4  147.    62  3.69  3.19  20       1     0     4     2
#>  9  25.6     4  141.    95  3.92  3.15  22.9     1     0     4     2
#> 10  17.1     6  168.   123  3.92  3.44  18.3     1     0     4     4

# ------------------------------------------------------------------------------

data(two_class_dat, package = "modeldata")
cls_trn <- two_class_dat[-(1:10), ]
cls_tst <- two_class_dat[  1:10 , ]

cls_form <-
  logistic_reg() |>
  set_engine("glm") |>
  fit(Class ~ ., data = cls_trn)
cls_xy <-
  logistic_reg() |>
  set_engine("glm") |>
  fit_xy(cls_trn[, -3],
  cls_trn$Class)

augment(cls_form, cls_tst)
#> # A tibble: 10 × 6
#>    .pred_class .pred_Class1 .pred_Class2     A     B Class 
#>    <fct>              <dbl>        <dbl> <dbl> <dbl> <fct> 
#>  1 Class1             0.518      0.482    2.07 1.63  Class1
#>  2 Class1             0.909      0.0913   2.02 1.04  Class1
#>  3 Class1             0.648      0.352    1.69 1.37  Class2
#>  4 Class1             0.610      0.390    3.43 1.98  Class2
#>  5 Class2             0.443      0.557    2.88 1.98  Class1
#>  6 Class2             0.206      0.794    3.31 2.41  Class2
#>  7 Class1             0.708      0.292    2.50 1.56  Class2
#>  8 Class1             0.567      0.433    1.98 1.55  Class2
#>  9 Class1             0.994      0.00582  2.88 0.580 Class1
#> 10 Class2             0.108      0.892    3.74 2.74  Class2
augment(cls_form, cls_tst[, -3])
#> # A tibble: 10 × 5
#>    .pred_class .pred_Class1 .pred_Class2     A     B
#>    <fct>              <dbl>        <dbl> <dbl> <dbl>
#>  1 Class1             0.518      0.482    2.07 1.63 
#>  2 Class1             0.909      0.0913   2.02 1.04 
#>  3 Class1             0.648      0.352    1.69 1.37 
#>  4 Class1             0.610      0.390    3.43 1.98 
#>  5 Class2             0.443      0.557    2.88 1.98 
#>  6 Class2             0.206      0.794    3.31 2.41 
#>  7 Class1             0.708      0.292    2.50 1.56 
#>  8 Class1             0.567      0.433    1.98 1.55 
#>  9 Class1             0.994      0.00582  2.88 0.580
#> 10 Class2             0.108      0.892    3.74 2.74 

augment(cls_xy, cls_tst)
#> # A tibble: 10 × 6
#>    .pred_class .pred_Class1 .pred_Class2     A     B Class 
#>    <fct>              <dbl>        <dbl> <dbl> <dbl> <fct> 
#>  1 Class1             0.518      0.482    2.07 1.63  Class1
#>  2 Class1             0.909      0.0913   2.02 1.04  Class1
#>  3 Class1             0.648      0.352    1.69 1.37  Class2
#>  4 Class1             0.610      0.390    3.43 1.98  Class2
#>  5 Class2             0.443      0.557    2.88 1.98  Class1
#>  6 Class2             0.206      0.794    3.31 2.41  Class2
#>  7 Class1             0.708      0.292    2.50 1.56  Class2
#>  8 Class1             0.567      0.433    1.98 1.55  Class2
#>  9 Class1             0.994      0.00582  2.88 0.580 Class1
#> 10 Class2             0.108      0.892    3.74 2.74  Class2
augment(cls_xy, cls_tst[, -3])
#> # A tibble: 10 × 5
#>    .pred_class .pred_Class1 .pred_Class2     A     B
#>    <fct>              <dbl>        <dbl> <dbl> <dbl>
#>  1 Class1             0.518      0.482    2.07 1.63 
#>  2 Class1             0.909      0.0913   2.02 1.04 
#>  3 Class1             0.648      0.352    1.69 1.37 
#>  4 Class1             0.610      0.390    3.43 1.98 
#>  5 Class2             0.443      0.557    2.88 1.98 
#>  6 Class2             0.206      0.794    3.31 2.41 
#>  7 Class1             0.708      0.292    2.50 1.56 
#>  8 Class1             0.567      0.433    1.98 1.55 
#>  9 Class1             0.994      0.00582  2.88 0.580
#> 10 Class2             0.108      0.892    3.74 2.74 

# ------------------------------------------------------------------------------

if (rlang::is_installed("quantreg")) {
  # Quantile regression example
  qr_form <-
    linear_reg() |>
    set_engine("quantreg") |>
    set_mode("quantile regression", quantile_levels = c(0.25, 0.5, 0.75)) |>
    fit(mpg ~ ., data = car_trn)

  augment(qr_form, car_tst)
  augment(qr_form, car_tst[, -1])
}
#> # A tibble: 10 × 11
#>    .pred_quantile   cyl  disp    hp  drat    wt  qsec    vs    am  gear
#>         <qtls(3)> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1         [22.9]     6  160    110  3.9   2.62  16.5     0     1     4
#>  2         [23.3]     6  160    110  3.9   2.88  17.0     0     1     4
#>  3         [31.3]     4  108     93  3.85  2.32  18.6     1     1     4
#>  4         [21.7]     6  258    110  3.08  3.22  19.4     1     0     3
#>  5         [15.6]     8  360    175  3.15  3.44  17.0     0     0     3
#>  6         [23.1]     6  225    105  2.76  3.46  20.2     1     0     3
#>  7         [14.2]     8  360    245  3.21  3.57  15.8     0     0     3
#>  8         [22.1]     4  147.    62  3.69  3.19  20       1     0     4
#>  9         [27.6]     4  141.    95  3.92  3.15  22.9     1     0     4
#> 10           [17]     6  168.   123  3.92  3.44  18.3     1     0     4
#> # ℹ 1 more variable: carb <dbl>
```
