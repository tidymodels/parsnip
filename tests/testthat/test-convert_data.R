skip_if_not_installed("modeldata")

hpc <- hpc_data[1:150, c(2:5, 8)]

# to go from lm_object$x results to our format
format_x_for_test <- function(x, df = TRUE) {
  x <- x[, colnames(x) != "(Intercept)", drop = FALSE]
  if (df)
    as.data.frame(x)
  else
    x
}

Puromycin_miss <- Puromycin
Puromycin_miss$state[20] <- NA
Puromycin_miss$conc[1] <- NA

# ------------------------------------------------------------------------------

# Testing formula -> xy conversion
test_that("numeric x and y", {
  expected <- lm(mpg ~ ., data = mtcars, x = TRUE, y = TRUE)
  observed <-
    .convert_form_to_xy_fit(
      mpg ~ .,
      data = mtcars,
      indicators = "traditional",
      remove_intercept = TRUE
    )
  expect_equal(format_x_for_test(expected$x), observed$x)
  expect_equal(mtcars$mpg, observed$y)
  expect_equal(expected$terms, observed$terms)
  expect_equal(expected$xlevels, observed$xlevels)
  expect_null(observed$weights)
  expect_null(observed$offset)

  expect_equal(
    mtcars[1:6,-1],
    .convert_form_to_xy_new(
      observed,
      new_data = head(mtcars))$x
  )
})

test_that("numeric x and y, subsetting", {
  expected <- lm(mpg ~ ., data = mtcars, subset = hp > 170,
                 x = TRUE, y = TRUE)
  observed <-
    .convert_form_to_xy_fit(
      mpg ~ .,
      data = mtcars,
      subset = hp > 170,
      indicators = "traditional",
      remove_intercept = TRUE
    )
  expect_equal(format_x_for_test(expected$x), observed$x)
  expect_equal(mtcars$mpg[mtcars$hp > 170], observed$y)
  expect_equal(expected$terms, observed$terms)
  expect_equal(expected$xlevels, observed$xlevels)
  expect_null(observed$weights)
  expect_null(observed$offset)
  # subset does not affect newdata calcs
})

test_that("numeric x and y, weights", {
  expected <- lm(mpg ~ . -disp, data = mtcars, weights = disp,
                 x = TRUE, y = TRUE)
  observed <-
    .convert_form_to_xy_fit(
      mpg ~ . - disp,
      data = mtcars,
      weights = disp,
      indicators = "traditional",
      remove_intercept = TRUE
    )
  expect_equal(format_x_for_test(expected$x), observed$x)
  expect_equal(mtcars$mpg, observed$y)
  expect_equal(expected$terms, observed$terms)
  expect_equal(expected$xlevels, observed$xlevels)
  expect_equal(mtcars$disp, observed$weights)
  expect_null(observed$offset)
})

test_that("numeric x and y, offset in-line", {
  expected <- lm(mpg ~ cyl + hp +  offset(log(disp)),
                 data = mtcars,
                 x = TRUE,
                 y = TRUE)
  observed <-
    .convert_form_to_xy_fit(
      mpg ~ cyl + hp +  offset(log(disp)),
      data = mtcars,
      indicators = "traditional",
      remove_intercept = TRUE
    )
  expect_equal(format_x_for_test(expected$x), observed$x)
  expect_equal(mtcars$mpg, observed$y)
  expect_equal(expected$terms, observed$terms)
  expect_equal(expected$xlevels, observed$xlevels)
  expect_equal(log(mtcars$disp), observed$offset)
  expect_null(observed$weights)

  new_obs <-
    .convert_form_to_xy_new(observed, new_data = mtcars[1:6, ])
  expect_equal(mtcars[1:6, c("cyl", "hp")], new_obs$x)
  expect_equal(log(mtcars$disp)[1:6], new_obs$offset)
})


test_that("numeric x and y, multiple offsets in-line", {
  expected <- lm(
    mpg ~ cyl + hp +  offset(log(disp)) + offset(qsec),
    data = mtcars,
    x = TRUE,
    y = TRUE
  )
  observed <-
    .convert_form_to_xy_fit(
      mpg ~ cyl + hp +  offset(log(disp)) +
        offset(qsec),
      data = mtcars,
      indicators = "traditional",
      remove_intercept = TRUE
    )
  expect_equal(format_x_for_test(expected$x), observed$x)
  expect_equal(mtcars$mpg, observed$y)
  expect_equal(expected$terms, observed$terms)
  expect_equal(expected$xlevels, observed$xlevels)
  expect_equal(log(mtcars$disp) + mtcars$qsec, observed$offset)
  expect_null(observed$weights)

  new_obs <-
    .convert_form_to_xy_new(observed, new_data = mtcars[1:6, ])
  expect_equal(mtcars[1:6, c("cyl", "hp")], new_obs$x)
  expect_equal(log(mtcars$disp)[1:6] + mtcars$qsec[1:6],
               new_obs$offset)
})

test_that("numeric x and y, no intercept", {
  expected <- lm(mpg ~ 0 + .,
                 data = mtcars,
                 x = TRUE,
                 y = TRUE)
  observed <-
    .convert_form_to_xy_fit(
      mpg ~ 0 + .,
      data = mtcars,
      indicators = "traditional",
      remove_intercept = TRUE
    )
  expect_equal(format_x_for_test(expected$x), observed$x)
  expect_equal(mtcars$mpg, observed$y)
  expect_equal(expected$terms, observed$terms)
  expect_equal(expected$xlevels, observed$xlevels)
  expect_null(observed$offset)
  expect_null(observed$weights)

  expect_equal(mtcars[1:6,-1],
               .convert_form_to_xy_new(observed, new_data = head(mtcars))$x)
})

test_that("numeric x and y, inline functions", {
  expected <- lm(log(mpg) ~ hp + poly(wt, 3),
                 data = mtcars,
                 x = TRUE,
                 y = TRUE)
  observed <-
    .convert_form_to_xy_fit(
      log(mpg) ~ hp + poly(wt, 3),
      data = mtcars,
      indicators = "traditional",
      remove_intercept = TRUE
    )
  expect_equal(format_x_for_test(expected$x), observed$x)
  expect_equal(log(mtcars$mpg), observed$y)
  expect_equal(expected$terms, observed$terms)
  expect_equal(expected$xlevels, observed$xlevels)
  expect_null(observed$offset)
  expect_null(observed$weights)

  expect_equal(
    format_x_for_test(model.matrix(expected$terms, head(mtcars))),
    .convert_form_to_xy_new(observed, new_data = head(mtcars))$x
  )
})

test_that("numeric y and mixed x", {
  expected <- lm(rate ~ ., data = Puromycin, x = TRUE, y = TRUE)
  observed <-
    .convert_form_to_xy_fit(
      rate ~ .,
      data = Puromycin,
      indicators = "traditional",
      remove_intercept = TRUE
    )
  expect_equal(format_x_for_test(expected$x), observed$x)
  expect_equal(Puromycin$rate, observed$y)
  expect_equal(expected$terms, observed$terms)
  expect_equal(expected$xlevels, observed$xlevels)
  expect_null(observed$weights)
  expect_null(observed$offset)

  expect_equal(
    format_x_for_test(model.matrix(expected$terms, head(Puromycin))),
    .convert_form_to_xy_new(observed, new_data = head(Puromycin))$x
  )
})

test_that("mixed x, no dummies, compare to a model that does not create dummies", {
  expected <- rpart::rpart(rate ~ ., data = Puromycin)
  data_classes <- attr(expected$terms, "dataClasses")[2:3]
  observed <-
    .convert_form_to_xy_fit(
      rate ~ .,
      data = Puromycin,
      indicators = "none",
      remove_intercept = TRUE
    )
  expect_equal(names(data_classes), names(observed$x))
  expect_equal(unname(data_classes), c("numeric", "factor"))
  expect_s3_class(observed$x$state, "factor")
  expect_equal(Puromycin$rate, observed$y)
  expect_equal(expected$terms, observed$terms)

  expect_null(observed$weights)
  expect_null(observed$offset)
})


test_that("numeric y and mixed x, omit missing data", {
  expected <- lm(rate ~ ., data = Puromycin_miss, x = TRUE, y = TRUE)
  observed <-
    .convert_form_to_xy_fit(
      rate ~ .,
      data = Puromycin_miss,
      indicators = "traditional",
      remove_intercept = TRUE
    )
  expect_equal(format_x_for_test(expected$x), observed$x)
  expect_equal(Puromycin_miss$rate[complete.cases(Puromycin_miss)],
                    observed$y)
  expect_equal(expected$terms, observed$terms)
  expect_equal(expected$xlevels, observed$xlevels)
  expect_null(observed$weights)
  expect_null(observed$offset)

  expect_equal(
    format_x_for_test(model.matrix(expected$terms, head(Puromycin_miss))),
    .convert_form_to_xy_new(
      observed,
      new_data = head(Puromycin_miss),
      na.action = na.omit
    )$x
  )
})

test_that("numeric y and mixed x, include missing data", {
  frame_obj <- model.frame(rate ~ .,
                           data = Puromycin_miss,
                           na.action = na.pass)
  expected <- model.matrix(rate ~ ., frame_obj)
  observed <- .convert_form_to_xy_fit(
    rate ~ .,
    data = Puromycin_miss,
    na.action = na.pass,
    indicators = "traditional",
    remove_intercept = TRUE
  )
  expect_equal(format_x_for_test(expected), observed$x)

  expect_equal(
    format_x_for_test(head(expected)),
    .convert_form_to_xy_new(
      observed,
      new_data = head(Puromycin_miss),
      na.action = na.pass
    )$x
  )
})

test_that("numeric y and mixed x, fail missing data", {
  expect_snapshot(
    error = TRUE,
    .convert_form_to_xy_fit(
      rate ~ .,
      data = Puromycin_miss,
      na.action = na.fail,
      indicators = "traditional",
      remove_intercept = TRUE
    )
  )
})

test_that("numeric y and mixed x, no dummies", {
  expected <- model.frame(rate ~ ., data = Puromycin)[,-1]
  observed <-
    .convert_form_to_xy_fit(
      rate ~ .,
      data = Puromycin,
      indicators = "none",
      remove_intercept = TRUE
    )
  expect_equal(expected, observed$x)

  expect_equal(
    format_x_for_test(head(expected)),
    .convert_form_to_xy_new(observed, new_data = head(Puromycin))$x
  )
})

test_that("numeric x and numeric multivariate y", {
  expected <- lm(cbind(mpg, disp) ~ .,
                 data = mtcars,
                 x = TRUE,
                 y = TRUE)
  observed <-
    .convert_form_to_xy_fit(
      cbind(mpg, disp) ~ .,
      data = mtcars,
      indicators = "traditional",
      remove_intercept = TRUE
    )
  expect_equal(format_x_for_test(expected$x), observed$x)
  expect_equal(mtcars[, c("mpg", "disp")], observed$y)
  expect_equal(expected$terms, observed$terms)
  expect_equal(expected$xlevels, observed$xlevels)
  expect_null(observed$weights)
  expect_null(observed$offset)

  expect_equal(mtcars[1:6,-c(1, 3)],
               .convert_form_to_xy_new(observed, new_data = head(mtcars))$x)
})

test_that("numeric x and factor y", {
  expect_snapshot(
    expected <-
      glm(class ~ ., data = hpc, x = TRUE, y = TRUE, family = binomial())
  )
  observed <- .convert_form_to_xy_fit(class ~ ., data = hpc)
  expect_equal(format_x_for_test(expected$x), observed$x)
  expect_equal(hpc$class, observed$y)
  expect_equal(expected$terms, observed$terms)
  expect_equal(expected$xlevels, observed$xlevels)
  expect_null(observed$weights)
  expect_null(observed$offset)

  expect_equal(
    head(format_x_for_test(expected$x)),
    .convert_form_to_xy_new(observed, new_data = head(hpc))$x
  )

  expect_no_error(
    observed2 <- .convert_form_to_xy_fit(class ~ ., data = hpc |> mutate(x = NA))
  )
  expect_equal(hpc$class[logical()], observed2$y)
  expect_s3_class(observed2$terms, "terms")
  expect_equal(expected$xlevels, observed2$xlevels)
  expect_null(observed2$weights)
  expect_null(observed2$offset)
})

test_that("bad args", {
  expect_snapshot(
    error = TRUE,
    .convert_form_to_xy_fit(
      mpg ~ ., data = mtcars,
      composition = "tibble",
      indicators = "traditional",
      remove_intercept = TRUE
    )
  )
  expect_snapshot(
    error = TRUE,
    .convert_form_to_xy_fit(
      mpg ~ ., data = mtcars,
      weights = letters[1:nrow(mtcars)],
      indicators = "traditional",
      remove_intercept = TRUE
    )
  )
})

test_that("numeric x and y, matrix composition", {
  expected <- lm(mpg ~ ., data = mtcars, x = TRUE, y = TRUE)
  observed <-
    .convert_form_to_xy_fit(
      mpg ~ .,
      data = mtcars,
      composition = "matrix",
      indicators = "traditional",
      remove_intercept = TRUE
    )
  expect_equal(format_x_for_test(expected$x, df = FALSE), observed$x)
  expect_equal(mtcars$mpg, observed$y)

  new_obs <-
    .convert_form_to_xy_new(observed,
                                     new_data = head(mtcars),
                                     composition = "matrix")
  expect_equal(as.matrix(mtcars[1:6,-1]), new_obs$x)
})

test_that("numeric x and multivariate y, matrix composition", {
  expected <-
    lm(cbind(mpg, cyl) ~ .,
       data = mtcars,
       x = TRUE,
       y = TRUE)
  observed <-
    .convert_form_to_xy_fit(
      cbind(mpg, cyl)  ~ .,
      data = mtcars,
      composition = "matrix",
      indicators = "traditional",
      remove_intercept = TRUE
    )
  expect_equal(format_x_for_test(expected$x, df = FALSE), observed$x)
  expect_equal(expected$y, observed$y)

  new_obs <-
    .convert_form_to_xy_new(observed,
                                     new_data = head(mtcars),
                                     composition = "matrix")
  expect_equal(as.matrix(mtcars[1:6,-(1:2)]), new_obs$x)
})

test_that("global `contrasts` option is respected", {
  contrasts <- getOption("contrasts")
  contrasts["unordered"] <- "contr.helmert"

  rlang::local_options(contrasts = contrasts)

  # Fit time
  fit_result <- .convert_form_to_xy_fit(
    num_pending ~ class + compounds,
    data = hpc
  )
  fit_data <- fit_result$x

  expect_identical(names(fit_data), c("class1", "class2", "class3", "compounds"))
  expect_true(all(fit_data$class1 %in% c(-1, 0, 1)))

  # Predict time
  predict_result <- .convert_form_to_xy_new(fit_result, hpc)
  predict_data <- predict_result$x

  expect_identical(names(predict_data), c("class1", "class2", "class3", "compounds"))
  expect_true(all(predict_data$class1 %in% c(-1, 0, 1)))
})

# ------------------------------------------------------------------------------

# Testing xy -> formula conversion
test_that("data frame x, vector y", {
  observed <-
    .convert_xy_to_form_fit(mtcars[, -1], mtcars$mpg, remove_intercept = TRUE)
  expected <- mtcars[, c(2:11, 1)]
  names(expected)[11] <- "..y"
  expect_equal(expected, observed$data)
  expect_equal(formula("..y ~ ."), observed$formula, ignore_formula_env = TRUE)
  expect_equal(names(mtcars)[-1], observed$x_var)
  expect_null(observed$weights)

  expect_equal(mtcars[1:6, -1],
               .convert_xy_to_form_new(observed, new_data = head(mtcars[,-1])))
})


test_that("matrix x, vector y", {
  observed <-
    .convert_xy_to_form_fit(as.matrix(mtcars[,-1]), mtcars$mpg,
                                     remove_intercept = TRUE)
  expected <- mtcars[, c(2:11, 1)]
  names(expected)[11] <- "..y"
  expect_equal(expected, observed$data)
  expect_equal(formula("..y ~ ."), observed$formula, ignore_formula_env = TRUE)
  expect_equal(names(mtcars)[-1], observed$x_var)
  expect_null(observed$weights)

  expect_equal(
    mtcars[1:6,-1],
    .convert_xy_to_form_new(observed, new_data = as.matrix(mtcars[1:6, -1]))
  )
})


test_that("data frame x, 1 col data frame y", {
  observed <-
    .convert_xy_to_form_fit(mtcars[, -1], mtcars[, "mpg", drop = FALSE],
                                     remove_intercept = TRUE)
  expected <- mtcars[, c(2:11, 1)]
  expect_equal(expected, observed$data)
  expect_equal(formula("mpg ~ ."), observed$formula, ignore_formula_env = TRUE)
  expect_equal(names(mtcars)[-1], observed$x_var)
  expect_null(observed$weights)
})

test_that("matrix x, 1 col matrix y", {
  observed <-
    .convert_xy_to_form_fit(as.matrix(mtcars[,-1]),
                                     as.matrix(mtcars[, "mpg", drop = FALSE]),
                                     remove_intercept = TRUE)
  expected <- mtcars[, c(2:11, 1)]
  expect_equal(expected, observed$data)
  expect_equal(formula("mpg ~ ."), observed$formula, ignore_formula_env = TRUE)
  expect_equal(names(mtcars)[-1], observed$x_var)
  expect_null(observed$weights)
})

test_that("matrix x, 1 col data frame y", {
  observed <-
    .convert_xy_to_form_fit(as.matrix(mtcars[,-1]),
                                     mtcars[, "mpg", drop = FALSE],
                                     remove_intercept = TRUE)
  expected <- mtcars[, c(2:11, 1)]
  expect_equal(expected, observed$data)
  expect_equal(formula("mpg ~ ."), observed$formula, ignore_formula_env = TRUE)
  expect_equal(names(mtcars)[-1], observed$x_var)
  expect_null(observed$weights)
})

test_that("data frame x, 1 col matrix y", {
  observed <-
    .convert_xy_to_form_fit(mtcars[,-1],
                                     as.matrix(mtcars[, "mpg", drop = FALSE]),
                                     remove_intercept = TRUE)
  expected <- mtcars[, c(2:11, 1)]
  expect_equal(expected, observed$data)
  expect_equal(formula("mpg ~ ."), observed$formula, ignore_formula_env = TRUE)
  expect_equal(names(mtcars)[-1], observed$x_var)
  expect_null(observed$weights)
})

test_that("data frame x, 2 col data frame y", {
  observed <-
    .convert_xy_to_form_fit(mtcars[,-(1:2)], mtcars[, 1:2],
                                     remove_intercept = TRUE)
  expected <- mtcars[, c(3:11, 1:2)]
  expect_equal(expected, observed$data)
  expect_equal(formula("cbind(mpg, cyl) ~ ."),
               observed$formula,
               ignore_formula_env = TRUE)
  expect_equal(names(mtcars)[-(1:2)], observed$x_var)
  expect_null(observed$weights)
})

test_that("matrix x, 2 col matrix y", {
  observed <-
    .convert_xy_to_form_fit(as.matrix(mtcars[,-(1:2)]),
                                     as.matrix(mtcars[, 1:2]),
                                     remove_intercept = TRUE)
  expected <- mtcars[, c(3:11, 1:2)]
  expect_equal(expected, observed$data)
  expect_equal(formula("cbind(mpg, cyl) ~ ."),
               observed$formula,
               ignore_formula_env = TRUE)
  expect_equal(names(mtcars)[-(1:2)], observed$x_var)
  expect_null(observed$weights)
})

test_that("1 col data frame x, 1 col data frame y", {
  observed <- .convert_xy_to_form_fit(mtcars[, 2, drop = FALSE],
                                               mtcars[, 1, drop = FALSE],
                                               remove_intercept = TRUE)
  expected <- mtcars[, 2:1]
  expect_equal(expected, observed$data)
  expect_equal(formula("mpg ~ ."), observed$formula, ignore_formula_env = TRUE)
  expect_equal(names(mtcars)[2], observed$x_var)
  expect_null(observed$weights)
})

# weights

test_that("1 col matrix x, 1 col matrix y", {
  observed <- .convert_xy_to_form_fit(
    as.matrix(mtcars[, 2, drop = FALSE]),
    as.matrix(mtcars[, 1, drop = FALSE]),
    remove_intercept = TRUE
  )
  expected <- mtcars[, 2:1]
  expect_equal(expected, observed$data)
  expect_equal(formula("mpg ~ ."), observed$formula, ignore_formula_env = TRUE)
  expect_equal(names(mtcars)[2], observed$x_var)
  expect_null(observed$weights)
})


test_that("matrix x, factor y", {
  observed <- .convert_xy_to_form_fit(as.matrix(hpc[, -5]), hpc$class)
  expected <- as.data.frame(hpc)
  names(expected)[5] <- "..y"
  expect_equal(expected, observed$data)
  expect_equal(formula("..y ~ ."), observed$formula, ignore_formula_env = TRUE)
  expect_equal(names(hpc)[-5], observed$x_var)
  expect_null(observed$weights)
})

test_that("data frame x, factor y", {
  observed <- .convert_xy_to_form_fit(hpc[, -5], hpc$class)
  expected <- hpc
  names(expected)[5] <- "..y"
  expect_equal(expected, observed$data)
  expect_equal(formula("..y ~ ."), observed$formula, ignore_formula_env = TRUE)
  expect_equal(names(hpc)[-5], observed$x_var)
  expect_null(observed$weights)
})


test_that("bad args", {
  expect_snapshot(
    error = TRUE,
    .convert_xy_to_form_fit(mtcars$disp, mtcars$mpg, remove_intercept = TRUE)
  )
  expect_snapshot(
    error = TRUE,
    .convert_xy_to_form_fit(mtcars[, 1:3], mtcars[, 2:5], remove_intercept = TRUE)
  )
})

## -----------------------------------------------------------------------------

test_that("convert to matrix", {
  skip_if_not_installed("modeldata")

  expect_true(inherits(parsnip::maybe_matrix(mtcars), "matrix"))
  expect_true(inherits(parsnip::maybe_matrix(tibble::as_tibble(mtcars)), "matrix"))
  expect_true(inherits(parsnip::maybe_matrix(as.matrix(mtcars)), "matrix"))
  expect_true(
    inherits(parsnip::maybe_matrix(Matrix::Matrix(as.matrix(mtcars), sparse = TRUE)),
             "dgCMatrix")
  )

  data(ames, package = "modeldata")
  expect_snapshot(
    error = TRUE,
    parsnip::maybe_matrix(ames[, c("Year_Built", "Neighborhood")])
  )
  # Also for date columns
  data(Chicago, package = "modeldata")
  expect_snapshot(
    error = TRUE,
    parsnip::maybe_matrix(Chicago[, c("ridership", "date")])
  )
})
