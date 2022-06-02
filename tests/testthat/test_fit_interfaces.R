hpc <- hpc_data[1:150, c(2:5, 8)]

f <- y ~ x

rmod <- linear_reg()

sprk <- 1:10
class(sprk) <- c(class(sprk), "tbl_spark")

tester <-
  function(object, formula = NULL,  data = NULL, model)
    parsnip:::check_interface(formula, data, match.call(expand.dots = TRUE), model)
tester_xy <-
  function(object, x = NULL, y = NULL, model)
    parsnip:::check_xy_interface(x, y, match.call(expand.dots = TRUE), model)


test_that('good args', {
  expect_equal(   tester(NULL, formula = f, data = hpc, model = rmod), "formula")
  expect_equal(tester_xy(NULL, x = hpc, y = hpc, model = rmod), "data.frame")
  expect_equal(   tester(NULL, f, data = hpc, model = rmod), "formula")
  expect_equal(   tester(NULL, f, data = sprk, model = rmod), "formula")
})

#test_that('unnamed args', {
#  expect_error(tester(NULL, hpc, y = hpc, model = rmod))
#  expect_error(tester(NULL, data = hpc, model = rmod))
#})
#
test_that('wrong args', {
 expect_error(tester_xy(NULL, x = sprk, y = hpc, model = rmod))
 expect_error(tester(NULL, f,  data = as.matrix(hpc[, 1:4])))
})

test_that('single column df for issue #129', {

  expect_error(
    lm1 <-
      linear_reg() %>%
      set_engine("lm") %>%
      fit_xy(x = mtcars[, 2:4], y = mtcars[,1, drop = FALSE]),
    regexp = NA
  )
  expect_error(
    lm2 <-
      linear_reg() %>%
      set_engine("lm") %>%
      fit_xy(x = mtcars[, 2:4], y = as.matrix(mtcars)[,1, drop = FALSE]),
    regexp = NA
  )
  lm3 <-
    linear_reg() %>%
    set_engine("lm") %>%
    fit_xy(x = mtcars[, 2:4], y = mtcars$mpg)
  expect_equal(coef(lm1), coef(lm3))
  expect_equal(coef(lm2), coef(lm3))
})

# ------------------------------------------------------------------------------

test_that('unknown modes', {
  mars_spec <- set_engine(mars(), "earth")
  expect_error(
    fit(mars_spec, am ~ ., data = mtcars),
    "Please set the mode in the model specification."
  )
  expect_error(
    fit_xy(mars_spec, x = mtcars[, -1], y = mtcars[,1]),
    regexp = NA
  )
  expect_error(
    fit_xy(mars_spec, x = lending_club[,1:2], y = lending_club$Class),
    regexp = NA
  )
})

test_that("elapsed time parsnip mods", {
  lm1 <-
    linear_reg() %>%
    set_engine("lm") %>%
    fit_xy(x = mtcars[, 2:4], y = mtcars$mpg,
           control = control_parsnip(verbosity = 2L))

  lm2 <-
    linear_reg() %>%
    set_engine("lm") %>%
    fit(mpg ~ ., data = mtcars,
        control = control_parsnip(verbosity = 2))

  expect_output(print(lm1), "Fit time:")
  expect_output(print(lm2), "Fit time:")
  expect_true(!is.null(lm1$elapsed))
  expect_true(!is.null(lm2$elapsed))

  lm3 <-
    linear_reg() %>%
    set_engine("lm") %>%
    fit_xy(x = mtcars[, 2:4], y = mtcars$mpg)

  output3 <- capture.output(print(lm3))

  expect_equal(sum(grepl("Fit time", output3)), 0)
})

test_that('No loaded engines', {
  expect_error(
    linear_reg() %>% fit(mpg ~., data = mtcars),
    regexp = NA
  )
  expect_error(
    expect_message(
      cubist_rules() %>% fit(mpg ~., data = mtcars)
    ),
    regexp = "Please load a parsnip extension package that provides one"
  )
  expect_error(
    expect_message(
      poisson_reg() %>% fit(mpg ~., data = mtcars)
    ),
    regexp = "Please load a parsnip extension package that provides one"
  )
})
