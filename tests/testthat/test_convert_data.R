library(testthat)
context("data conversion")
library(parsnip)


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

context("Testing formula -> xy conversion")

test_that("numeric x and y", {
  expected <- lm(mpg ~ ., data = mtcars, x = TRUE, y = TRUE)
  observed <- parsnip:::convert_form_to_xy_fit(mpg ~ ., data = mtcars)
  expect_equal(format_x_for_test(expected$x), observed$x)
  expect_equivalent(mtcars$mpg, observed$y)
  expect_equal(expected$terms, observed$terms)
  expect_equal(expected$xlevels, observed$xlevels)
  expect_null(observed$weights)
  expect_null(observed$offset)

  expect_equal(
    mtcars[1:6, -1],
    parsnip:::convert_form_to_xy_new(observed, new_data = head(mtcars))$x
  )
})

test_that("numeric x and y, subsetting", {
  expected <- lm(mpg ~ ., data = mtcars, subset = hp > 170,
                 x = TRUE, y = TRUE)
  observed <- parsnip:::convert_form_to_xy_fit(mpg ~ ., data = mtcars,
                                               subset = hp > 170)
  expect_equal(format_x_for_test(expected$x), observed$x)
  expect_equivalent(mtcars$mpg[mtcars$hp > 170], observed$y)
  expect_equal(expected$terms, observed$terms)
  expect_equal(expected$xlevels, observed$xlevels)
  expect_null(observed$weights)
  expect_null(observed$offset)
  # subset does not affect newdata calcs
})

test_that("numeric x and y, weights", {
  expected <- lm(mpg ~ . -disp, data = mtcars, weights = disp,
                 x = TRUE, y = TRUE)
  observed <- parsnip:::convert_form_to_xy_fit(mpg ~ . -disp, data = mtcars,
                                               weights = disp)
  expect_equal(format_x_for_test(expected$x), observed$x)
  expect_equivalent(mtcars$mpg, observed$y)
  expect_equal(expected$terms, observed$terms)
  expect_equal(expected$xlevels, observed$xlevels)
  expect_equal(mtcars$disp, observed$weights)
  expect_null(observed$offset)
})

test_that("numeric x and y, offset", {
  expected <- lm(mpg ~ . -disp, data = mtcars, offset = disp,
                 x = TRUE, y = TRUE)
  observed <- parsnip:::convert_form_to_xy_fit(mpg ~ . -disp, data = mtcars,
                                               offset = log(disp))
  expect_equal(format_x_for_test(expected$x), observed$x)
  expect_equivalent(mtcars$mpg, observed$y)
  expect_equal(expected$terms, observed$terms)
  expect_equal(expected$xlevels, observed$xlevels)
  expect_equal(log(mtcars$disp), observed$offset)
  expect_null(observed$weights)

  new_obs <- parsnip:::convert_form_to_xy_new(observed, new_data = mtcars[1:6,])
  expect_equal(mtcars[1:6,-c(1, 3)], new_obs$x)
  expect_equal(log(mtcars$disp)[1:6], new_obs$offset)
})

test_that("numeric x and y, offset in-line", {
  expected <- lm(mpg ~ cyl + hp +  offset(log(disp)), data = mtcars,
                 x = TRUE, y = TRUE)
  observed <- parsnip:::convert_form_to_xy_fit(mpg ~ cyl + hp +  offset(log(disp)),
                                               data = mtcars)
  expect_equal(format_x_for_test(expected$x), observed$x)
  expect_equivalent(mtcars$mpg, observed$y)
  expect_equal(expected$terms, observed$terms)
  expect_equal(expected$xlevels, observed$xlevels)
  expect_equal(log(mtcars$disp), observed$offset)
  expect_null(observed$weights)

  new_obs <- parsnip:::convert_form_to_xy_new(observed, new_data = mtcars[1:6,])
  expect_equal(mtcars[1:6, c("cyl", "hp")], new_obs$x)
  expect_equal(log(mtcars$disp)[1:6], new_obs$offset)
})


test_that("numeric x and y, multiple offsets in-line", {
  expected <- lm(mpg ~ cyl + hp +  offset(log(disp)) + offset(qsec),
                 data = mtcars,
                 x = TRUE, y = TRUE)
  observed <- parsnip:::convert_form_to_xy_fit(mpg ~ cyl + hp +  offset(log(disp)) +
                                                 offset(qsec), data = mtcars)
  expect_equal(format_x_for_test(expected$x), observed$x)
  expect_equivalent(mtcars$mpg, observed$y)
  expect_equal(expected$terms, observed$terms)
  expect_equal(expected$xlevels, observed$xlevels)
  expect_equal(log(mtcars$disp) + mtcars$qsec, observed$offset)
  expect_null(observed$weights)

  new_obs <- parsnip:::convert_form_to_xy_new(observed, new_data = mtcars[1:6,])
  expect_equal(mtcars[1:6, c("cyl", "hp")], new_obs$x)
  expect_equal(log(mtcars$disp)[1:6] + mtcars$qsec[1:6],
               new_obs$offset)
})

test_that("numeric x and y, no intercept", {
  expected <- lm(mpg ~ 0 + ., data = mtcars,
                 x = TRUE, y = TRUE)
  observed <- parsnip:::convert_form_to_xy_fit(mpg ~ 0 + ., data = mtcars)
  expect_equal(format_x_for_test(expected$x), observed$x)
  expect_equivalent(mtcars$mpg, observed$y)
  expect_equal(expected$terms, observed$terms)
  expect_equal(expected$xlevels, observed$xlevels)
  expect_null(observed$offset)
  expect_null(observed$weights)

  expect_equal(
    mtcars[1:6, -1],
    parsnip:::convert_form_to_xy_new(observed, new_data = head(mtcars))$x
  )
})

test_that("numeric x and y, inline functions", {
  expected <- lm(log(mpg) ~ hp + poly(wt, 3), data = mtcars,
                 x = TRUE, y = TRUE)
  observed <- parsnip:::convert_form_to_xy_fit(log(mpg) ~ hp + poly(wt, 3),
                                               data = mtcars)
  expect_equal(format_x_for_test(expected$x), observed$x)
  expect_equivalent(log(mtcars$mpg), observed$y)
  expect_equal(expected$terms, observed$terms)
  expect_equal(expected$xlevels, observed$xlevels)
  expect_null(observed$offset)
  expect_null(observed$weights)

  expect_equal(
    format_x_for_test(model.matrix(expected$terms, head(mtcars))),
    parsnip:::convert_form_to_xy_new(observed, new_data = head(mtcars))$x
  )
})

test_that("numeric y and mixed x", {
  expected <- lm(rate ~ ., data = Puromycin, x = TRUE, y = TRUE)
  observed <- parsnip:::convert_form_to_xy_fit(rate ~ ., data = Puromycin)
  expect_equal(format_x_for_test(expected$x), observed$x)
  expect_equivalent(Puromycin$rate, observed$y)
  expect_equal(expected$terms, observed$terms)
  expect_equal(expected$xlevels, observed$xlevels)
  expect_null(observed$weights)
  expect_null(observed$offset)

  expect_equal(
    format_x_for_test(model.matrix(expected$terms, head(Puromycin))),
    parsnip:::convert_form_to_xy_new(observed, new_data = head(Puromycin))$x
  )
})

test_that("numeric y and mixed x, omit missing data", {
  expected <- lm(rate ~ ., data = Puromycin_miss, x = TRUE, y = TRUE)
  observed <- parsnip:::convert_form_to_xy_fit(rate ~ ., data = Puromycin_miss)
  expect_equal(format_x_for_test(expected$x), observed$x)
  expect_equivalent(
    Puromycin_miss$rate[complete.cases(Puromycin_miss)],
    observed$y
  )
  expect_equal(expected$terms, observed$terms)
  expect_equal(expected$xlevels, observed$xlevels)
  expect_null(observed$weights)
  expect_null(observed$offset)

  expect_equal(
    format_x_for_test(model.matrix(expected$terms, head(Puromycin_miss))),
    parsnip:::convert_form_to_xy_new(observed, new_data = head(Puromycin_miss),
                                     na.action = na.omit)$x
  )
})

test_that("numeric y and mixed x, include missing data", {
  frame_obj <- model.frame(rate ~ .,
                           data = Puromycin_miss,
                           na.action = na.pass)
  expected <- model.matrix(rate ~ ., frame_obj)
  observed <- parsnip:::convert_form_to_xy_fit(rate ~ .,
                                               data = Puromycin_miss,
                                               na.action = na.pass)
  expect_equal(format_x_for_test(expected), observed$x)

  expect_equal(
    format_x_for_test(head(expected)),
    parsnip:::convert_form_to_xy_new(observed, new_data = head(Puromycin_miss),
                                     na.action = na.pass)$x
  )
})

test_that("numeric y and mixed x, fail missing data", {
  expect_error(
    parsnip:::convert_form_to_xy_fit(rate ~ .,
                                     data = Puromycin_miss,
                                     na.action = na.fail)
  )
})

test_that("numeric y and mixed x, no dummies", {
  expected <- model.frame(rate ~ ., data = Puromycin)[, -1]
  observed <-
    parsnip:::convert_form_to_xy_fit(rate ~ ., data = Puromycin, indicators = FALSE)
  expect_equivalent(expected, observed$x)

  expect_equal(
    format_x_for_test(head(expected)),
    parsnip:::convert_form_to_xy_new(observed, new_data = head(Puromycin))$x
  )
})

test_that("numeric x and numeric multivariate y", {
  expected <- lm(cbind(mpg, disp) ~ ., data = mtcars,
                 x = TRUE, y = TRUE)
  observed <- parsnip:::convert_form_to_xy_fit(cbind(mpg, disp) ~ ., data = mtcars)
  expect_equal(format_x_for_test(expected$x), observed$x)
  expect_equivalent(mtcars[, c("mpg", "disp")], observed$y)
  expect_equal(expected$terms, observed$terms)
  expect_equal(expected$xlevels, observed$xlevels)
  expect_null(observed$weights)
  expect_null(observed$offset)

  expect_equal(
    mtcars[1:6, -c(1, 3)],
    parsnip:::convert_form_to_xy_new(observed, new_data = head(mtcars))$x
  )
})

test_that("numeric x and factor y", {
  expected <-
    expect_warning(
      glm(Species ~ ., data = iris, x = TRUE, y = TRUE, family = binomial()
      )
    )
  observed <- parsnip:::convert_form_to_xy_fit(Species ~ ., data = iris)
  expect_equal(format_x_for_test(expected$x), observed$x)
  expect_equivalent(iris$Species, observed$y)
  expect_equal(expected$terms, observed$terms)
  expect_equal(expected$xlevels, observed$xlevels)
  expect_null(observed$weights)
  expect_null(observed$offset)

  expect_equal(
    head(format_x_for_test(expected$x)),
    parsnip:::convert_form_to_xy_new(observed, new_data = head(iris))$x
  )

})

test_that("bad args", {
  expect_error(
    parsnip:::convert_form_to_xy_fit(
      mpg ~ ., data = mtcars,
      composition = "tibble"
    )
  )
  expect_error(
    parsnip:::convert_form_to_xy_fit(
      mpg ~ ., data = mtcars,
      weights = letters[1:nrow(mtcars)]
    )
  )
  expect_error(
    parsnip:::convert_form_to_xy_fit(
      mpg ~ ., data = mtcars,
      offset = 1:10
    )
  )
})

test_that("numeric x and y, matrix composition", {
  expected <- lm(mpg ~ ., data = mtcars, x = TRUE, y = TRUE)
  observed <- parsnip:::convert_form_to_xy_fit(mpg ~ ., data = mtcars,
                                               composition = "matrix")
  expect_equal(format_x_for_test(expected$x, df = FALSE), observed$x)
  expect_equivalent(mtcars$mpg, observed$y)

  new_obs <- parsnip:::convert_form_to_xy_new(observed, new_data = head(mtcars),
                                              composition = "matrix")
  expect_equal(as.matrix(mtcars[1:6, -1]), new_obs$x)
})

test_that("numeric x and multivariate y, matrix composition", {
  expected <- lm(cbind(mpg, cyl) ~ ., data = mtcars, x = TRUE, y = TRUE)
  observed <- parsnip:::convert_form_to_xy_fit(cbind(mpg, cyl)  ~ ., data = mtcars,
                                               composition = "matrix")
  expect_equal(format_x_for_test(expected$x, df = FALSE), observed$x)
  expect_equivalent(expected$y, observed$y)

  new_obs <- parsnip:::convert_form_to_xy_new(observed, new_data = head(mtcars),
                                              composition = "matrix")
  expect_equal(as.matrix(mtcars[1:6, -(1:2)]), new_obs$x)
})

# ------------------------------------------------------------------------------

context("Testing xy -> formula conversion")

test_that("data frame x, vector y", {
  observed <- parsnip:::convert_xy_to_form_fit(mtcars[, -1], mtcars$mpg)
  expected <- mtcars[, c(2:11, 1)]
  names(expected)[11] <- "..y"
  expect_equal(expected, observed$data)
  expect_equal(formula("..y ~ ."), observed$formula)
  expect_equal(names(mtcars)[-1], observed$x_var)
  expect_null(observed$weights)

  expect_equal(
    mtcars[1:6, -1],
    parsnip:::convert_xy_to_form_new(observed, new_data = head(mtcars[,-1]))
  )
})


test_that("matrix x, vector y", {
  observed <- parsnip:::convert_xy_to_form_fit(as.matrix(mtcars[, -1]), mtcars$mpg)
  expected <- mtcars[, c(2:11, 1)]
  names(expected)[11] <- "..y"
  expect_equal(expected, observed$data)
  expect_equal(formula("..y ~ ."), observed$formula)
  expect_equal(names(mtcars)[-1], observed$x_var)
  expect_null(observed$weights)

  expect_equal(
    mtcars[1:6, -1],
    parsnip:::convert_xy_to_form_new(observed, new_data = as.matrix(mtcars[1:6,-1]))
  )
})


test_that("data frame x, 1 col data frame y", {
  observed <- parsnip:::convert_xy_to_form_fit(mtcars[, -1], mtcars[, "mpg", drop = FALSE])
  expected <- mtcars[, c(2:11, 1)]
  expect_equal(expected, observed$data)
  expect_equal(formula("mpg ~ ."), observed$formula)
  expect_equal(names(mtcars)[-1], observed$x_var)
  expect_null(observed$weights)
})

test_that("matrix x, 1 col matrix y", {
  observed <-
    parsnip:::convert_xy_to_form_fit(as.matrix(mtcars[,-1]),
                                     as.matrix(mtcars[, "mpg", drop = FALSE]))
  expected <- mtcars[, c(2:11, 1)]
  expect_equal(expected, observed$data)
  expect_equal(formula("mpg ~ ."), observed$formula)
  expect_equal(names(mtcars)[-1], observed$x_var)
  expect_null(observed$weights)
})

test_that("matrix x, 1 col data frame y", {
  observed <- parsnip:::convert_xy_to_form_fit(as.matrix(mtcars[, -1]),
                                               mtcars[, "mpg", drop = FALSE])
  expected <- mtcars[, c(2:11, 1)]
  expect_equal(expected, observed$data)
  expect_equal(formula("mpg ~ ."), observed$formula)
  expect_equal(names(mtcars)[-1], observed$x_var)
  expect_null(observed$weights)
})

test_that("data frame x, 1 col matrix y", {
  observed <-
    parsnip:::convert_xy_to_form_fit(mtcars[,-1], as.matrix(mtcars[, "mpg", drop = FALSE]))
  expected <- mtcars[, c(2:11, 1)]
  expect_equal(expected, observed$data)
  expect_equal(formula("mpg ~ ."), observed$formula)
  expect_equal(names(mtcars)[-1], observed$x_var)
  expect_null(observed$weights)
})

test_that("data frame x, 2 col data frame y", {
  observed <- parsnip:::convert_xy_to_form_fit(mtcars[, -(1:2)], mtcars[, 1:2])
  expected <- mtcars[, c(3:11, 1:2)]
  expect_equal(expected, observed$data)
  expect_equal(formula("cbind(mpg, cyl) ~ ."), observed$formula)
  expect_equal(names(mtcars)[-(1:2)], observed$x_var)
  expect_null(observed$weights)
})

test_that("matrix x, 2 col matrix y", {
  observed <-
    parsnip:::convert_xy_to_form_fit(as.matrix(mtcars[,-(1:2)]),
                                     as.matrix(mtcars[, 1:2]))
  expected <- mtcars[, c(3:11, 1:2)]
  expect_equal(expected, observed$data)
  expect_equal(formula("cbind(mpg, cyl) ~ ."), observed$formula)
  expect_equal(names(mtcars)[-(1:2)], observed$x_var)
  expect_null(observed$weights)
})

test_that("1 col data frame x, 1 col data frame y", {
  observed <- parsnip:::convert_xy_to_form_fit(mtcars[, 2, drop = FALSE],
                                               mtcars[, 1, drop = FALSE])
  expected <- mtcars[, 2:1]
  expect_equal(expected, observed$data)
  expect_equal(formula("mpg ~ ."), observed$formula)
  expect_equal(names(mtcars)[2], observed$x_var)
  expect_null(observed$weights)
})

# weights

test_that("1 col matrix x, 1 col matrix y", {
  observed <- parsnip:::convert_xy_to_form_fit(
    as.matrix(mtcars[, 2, drop = FALSE]),
    as.matrix(mtcars[, 1, drop = FALSE])
  )
  expected <- mtcars[, 2:1]
  expect_equal(expected, observed$data)
  expect_equal(formula("mpg ~ ."), observed$formula)
  expect_equal(names(mtcars)[2], observed$x_var)
  expect_null(observed$weights)
})


test_that("matrix x, factor y", {
  observed <- parsnip:::convert_xy_to_form_fit(as.matrix(iris[, -5]), iris$Species)
  expected <- iris
  names(expected)[5] <- "..y"
  expect_equal(expected, observed$data)
  expect_equal(formula("..y ~ ."), observed$formula)
  expect_equal(names(iris)[-5], observed$x_var)
  expect_null(observed$weights)
})

test_that("data frame x, factor y", {
  observed <- parsnip:::convert_xy_to_form_fit(iris[, -5], iris$Species)
  expected <- iris
  names(expected)[5] <- "..y"
  expect_equivalent(expected, observed$data)
  expect_equal(formula("..y ~ ."), observed$formula)
  expect_equal(names(iris)[-5], observed$x_var)
  expect_null(observed$weights)
})


test_that("bad args", {
  expect_error(
    parsnip:::convert_xy_to_form_fit(mtcars$disp, mtcars$mpg)
  )
  expect_error(
    parsnip:::convert_xy_to_form_fit(mtcars[, 1:3], mtcars[, 2:5])
  )
})

