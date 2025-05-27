skip_if_not_installed("modeldata")

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
#  expect_snapshot(error = TRUE, tester(NULL, hpc, y = hpc, model = rmod))
#  expect_snapshot(error = TRUE, tester(NULL, data = hpc, model = rmod))
#})
#
test_that('wrong args', {
 expect_snapshot(error = TRUE, tester_xy(NULL, x = sprk, y = hpc, model = rmod))
 expect_snapshot(error = TRUE, tester(NULL, f,  data = as.matrix(hpc[, 1:4])))
})

test_that('single column df for issue #129', {

  expect_no_condition(
    lm1 <-
      linear_reg() |>
      set_engine("lm") |>
      fit_xy(x = mtcars[, 2:4], y = mtcars[,1, drop = FALSE])
  )
  expect_no_condition(
    lm2 <-
      linear_reg() |>
      set_engine("lm") |>
      fit_xy(x = mtcars[, 2:4], y = as.matrix(mtcars)[,1, drop = FALSE])
  )
  lm3 <-
    linear_reg() |>
    set_engine("lm") |>
    fit_xy(x = mtcars[, 2:4], y = mtcars$mpg)
  expect_equal(coef(lm1), coef(lm3))
  expect_equal(coef(lm2), coef(lm3))
})

# ------------------------------------------------------------------------------

test_that('unknown modes', {
  mars_spec <- set_engine(mars(), "earth")
  expect_snapshot(
    error = TRUE,
    fit(mars_spec, am ~ ., data = mtcars)
  )
  expect_snapshot(
    error = TRUE,
    fit_xy(mars_spec, x = mtcars[, -1], y = mtcars[,1])
  )
  expect_snapshot(
    error = TRUE,
    fit_xy(mars_spec, x = lending_club[,1:2], y = lending_club$Class)
  )
})

test_that("misspecified formula argument", {
  rec <- structure(list(), class = "recipe")
  expect_snapshot(error = TRUE,
    fit(linear_reg(), rec, mtcars)
  )
  expect_snapshot(error = TRUE,
    fit(linear_reg(), "boop", mtcars)
  )
})

test_that("elapsed time parsnip mods", {
  lm1 <-
    linear_reg() |>
    set_engine("lm") |>
    fit_xy(x = mtcars[, 2:4], y = mtcars$mpg,
           control = control_parsnip(verbosity = 2L))

  lm2 <-
    linear_reg() |>
    set_engine("lm") |>
    fit(mpg ~ ., data = mtcars,
        control = control_parsnip(verbosity = 2))

  expect_output(print(lm1), "Fit time:")
  expect_output(print(lm2), "Fit time:")
  expect_true(!is.null(lm1$elapsed))
  expect_true(!is.null(lm2$elapsed))

  lm3 <-
    linear_reg() |>
    set_engine("lm") |>
    fit_xy(x = mtcars[, 2:4], y = mtcars$mpg)

  output3 <- capture.output(print(lm3))

  expect_equal(sum(grepl("Fit time", output3)), 0)
})

test_that('No loaded engines', {
  expect_no_condition(
    linear_reg() |> fit(mpg ~., data = mtcars)
  )
  expect_snapshot_error({cubist_rules() |> fit(mpg ~., data = mtcars)})
  expect_snapshot_error({poisson_reg() |> fit(mpg ~., data = mtcars)})
  expect_snapshot_error({cubist_rules(engine = "Cubist") |> fit(mpg ~., data = mtcars)})
})

test_that("fit_xy() can handle attributes on a data.frame outcome (#1060)", {
  lr <- linear_reg()
  x <- data.frame(x = 1:5)
  y <- c(2:5, 5)

  expect_silent(res <-
                  fit_xy(lr, x = x, y =  data.frame(y = structure(y, label = "hi")))
  )
  expect_equal(res[["fit"]], fit_xy(lr, x, y)[["fit"]], ignore_attr = "label")
})

test_that("fit_xy() can handle attributes on an atomic outcome (#1061)", {
  lr <- linear_reg()
  x <- data.frame(x = 1:5)
  y <- c(2:5, 5)

  expect_silent(res <- fit_xy(lr, x = x, y = structure(y, label = "hi")))
  expect_equal(res[["fit"]], fit_xy(lr, x, y)[["fit"]], ignore_attr = "label")
})

test_that("fit() can handle attributes on a vector outcome", {
  lr <- linear_reg()
  dat <- data.frame(x = 1:5, y = c(2:5, 5))
  dat_attr <- data.frame(x = 1:5, y = structure(c(2:5, 5), label = "hi"))

  expect_silent(res <- fit(lr, y ~ x, dat_attr))
  expect_equal(
    res[["fit"]],
    fit(lr, y ~ x, dat)[["fit"]],
    ignore_attr = TRUE
  )
})

test_that("overhead of parsnip interface is minimal (#1071)", {
  skip_on_cran()
  skip_on_covr()
  skip_if_not_installed("bench")
  skip_if_not_installed("parsnip", minimum_version = "1.4.0")

  bm <- bench::mark(
    time_engine = lm(mpg ~ ., mtcars),
    time_parsnip_form = fit(linear_reg(), mpg ~ ., mtcars),
    time_parsnip_xy = fit_xy(linear_reg(), mtcars[2:11],  mtcars[1]),
    relative = TRUE,
    check = FALSE
  )

  expect_true(
    bm$median[2] < 3.5,
    label = paste0("parsnip overhead factor (formula interface): ", round(bm$median[2], 4))
  )
  expect_true(
    bm$median[3] < 3.75,
    label = paste0("parsnip overhead factor (xy interface): ", round(bm$median[3], 4))
  )
})
