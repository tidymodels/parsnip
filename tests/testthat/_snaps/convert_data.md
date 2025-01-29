# numeric y and mixed x, fail missing data

    Code
      .convert_form_to_xy_fit(rate ~ ., data = Puromycin_miss, na.action = na.fail,
      indicators = "traditional", remove_intercept = TRUE)
    Condition
      Error in `na.fail.default()`:
      ! missing values in object

# numeric x and factor y

    Code
      expected <- glm(class ~ ., data = hpc, x = TRUE, y = TRUE, family = binomial())
    Condition
      Warning:
      glm.fit: fitted probabilities numerically 0 or 1 occurred

# bad args

    Code
      .convert_form_to_xy_fit(mpg ~ ., data = mtcars, composition = "tibble",
      indicators = "traditional", remove_intercept = TRUE)
    Condition
      Error:
      ! `composition` should be either "data.frame", "matrix", or "dgCMatrix".

---

    Code
      .convert_form_to_xy_fit(mpg ~ ., data = mtcars, weights = letters[1:nrow(mtcars)],
      indicators = "traditional", remove_intercept = TRUE)
    Condition
      Error:
      ! `weights` must be a numeric vector.

---

    Code
      .convert_xy_to_form_fit(mtcars$disp, mtcars$mpg, remove_intercept = TRUE)
    Condition
      Error:
      ! `x` cannot be a vector.

---

    Code
      .convert_xy_to_form_fit(mtcars[, 1:3], mtcars[, 2:5], remove_intercept = TRUE)
    Condition
      Error in `.convert_xy_to_form_fit()`:
      ! `x` and `y` have the names "cyl" and "disp" in common.
      i Please ensure that `x` and `y` don't share any column names.

# convert to matrix

    Code
      parsnip::maybe_matrix(ames[, c("Year_Built", "Neighborhood")])
    Condition
      Error in `parsnip::maybe_matrix()`:
      ! The column "Neighborhood" is non-numeric, so the data cannot be converted to a numeric matrix.

---

    Code
      parsnip::maybe_matrix(Chicago[, c("ridership", "date")])
    Condition
      Error in `parsnip::maybe_matrix()`:
      ! The column "date" is non-numeric, so the data cannot be converted to a numeric matrix.

