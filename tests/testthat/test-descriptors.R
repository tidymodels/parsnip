skip_if_not_installed("modeldata")

hpc <- hpc_data[1:150, c(2:5, 8)] |> as.data.frame()

# ------------------------------------------------------------------------------

template <- function(col, pred, ob, lev, fact, dat, x, y) {
  lst <- list(.cols = col, .preds = pred, .obs = ob,
              .lvls = lev, .facts = fact, .dat = dat,
              .x = x, .y = y)

  Filter(Negate(is.null), lst)
}

eval_descrs <- function(descrs, not = NULL) {

  if (!is.null(not)) {
    for (descr in not) {
      descrs[[descr]] <- NULL
    }
  }

  lapply(descrs, do.call, list())
}

class_tab <- table(hpc$class, dnn = NULL)

# ------------------------------------------------------------------------------

# Should descriptors be created?

test_that("requires_descrs", {

  # embedded in a function
  fn <- function() {
    .cols()
  }

  # doubly embedded
  fn2 <- function() {
    fn()
  }

  # core args
  expect_false(parsnip:::requires_descrs(rand_forest()))
  expect_false(parsnip:::requires_descrs(rand_forest(mtry = 3)))
  expect_false(parsnip:::requires_descrs(rand_forest(mtry = tune())))
  expect_true(parsnip:::requires_descrs(rand_forest(mtry = .cols())))
  expect_false(parsnip:::requires_descrs(rand_forest(mtry = expr(3))))
  expect_false(parsnip:::requires_descrs(rand_forest(mtry = quote(3))))
  expect_true(parsnip:::requires_descrs(rand_forest(mtry = fn())))
  expect_true(parsnip:::requires_descrs(rand_forest(mtry = fn2())))

  # descriptors in `eng_args`
  expect_false(parsnip:::requires_descrs(rand_forest() |> set_engine("ranger", arrrg = 3)))
  expect_false(parsnip:::requires_descrs(rand_forest() |> set_engine("ranger", arrrg = tune())))
  expect_true(parsnip:::requires_descrs(rand_forest() |> set_engine("ranger", arrrg = .obs())))
  expect_false(parsnip:::requires_descrs(rand_forest() |> set_engine("ranger", arrrg = expr(3))))
  expect_true(parsnip:::requires_descrs(rand_forest() |> set_engine("ranger", arrrg = fn())))
  expect_true(parsnip:::requires_descrs(rand_forest() |> set_engine("ranger", arrrg = fn2())))

  # mixed
  expect_true(
    parsnip:::requires_descrs(
      rand_forest(mtry = 3) |> set_engine("ranger", arrrg = fn2())
    )
  )

  expect_true(
    parsnip:::requires_descrs(
      rand_forest(mtry = .cols()) |> set_engine("ranger", arrrg = 3)
    )
  )
})


# ------------------------------------------------------------------------------

# Testing formula -> xy conversion

test_that("numeric y and dummy vars", {
  expect_equal(
    template(6, 4, 150, NA, 1, hpc, hpc[-2], hpc[,"input_fields"]),
    eval_descrs(get_descr_form(input_fields ~ ., data = hpc))
  )
  expect_equal(
    template(3, 1, 150, NA, 1, hpc, hpc["class"], hpc[,"input_fields"]),
    eval_descrs(get_descr_form(input_fields ~ class, data = hpc))
  )
})

test_that("numeric y and x", {
  expect_equal(
    template(1, 1, 150, NA, 0, hpc, hpc["input_fields"], hpc[,"compounds"]),
    eval_descrs(get_descr_form(compounds ~ input_fields, data = hpc))
  )
  expect_equal(
    {
      log_sep <- hpc["input_fields"]
      log_sep[["input_fields"]] <- log(log_sep[["input_fields"]])
      names(log_sep) <- "log(input_fields)"
      template(1, 1, 150, NA, 0, hpc, log_sep, hpc[,"compounds"])
    },
    eval_descrs(get_descr_form(compounds ~ log(input_fields), data = hpc))
  )
})

test_that("factor y", {
  expect_equal(
    template(4, 4, 150, class_tab, 0, hpc, hpc[-5], hpc[,"class"]),
    eval_descrs(get_descr_form(class ~ ., data = hpc))
  )
  expect_equal(
    template(1, 1, 150, class_tab, 0, hpc, hpc["compounds"], hpc[,"class"]),
    eval_descrs(get_descr_form(class ~ compounds, data = hpc))
  )
})

test_that("factors all the way down", {
  dat <- npk[,1:4]
  expect_equal(
    template(7, 3, 24, table(npk$K, dnn = NULL), 3, dat, dat[-4], dat[,"K"]),
    eval_descrs(get_descr_form(K ~ ., data = dat))
  )
})

test_that("weird cases", {
  # So model.frame ignores - signs in a model formula so class is not removed
  # prior to model.matrix; otherwise this should have n_cols = 3
  expect_equal(
    template(3, 4, 150, NA, 1, hpc, hpc[-2], hpc[,"input_fields"]),
    eval_descrs(get_descr_form(input_fields ~ . - class, data = hpc))
  )

  # Oy ve! Before going to model.matrix, model.frame produces a data frame
  # with one column and that column is a matrix (with the results from
  # `poly(input_fields, 3)`
  x <- model.frame(~poly(input_fields, 3), hpc)
  attributes(x) <- attributes(as.data.frame(x))[c("names", "class", "row.names")]
  expect_equal(
    template(3, 1, 150, NA, 0, hpc, x, hpc[,"compounds"]),
    eval_descrs(get_descr_form(compounds ~ poly(input_fields, 3), data = hpc))
  )

  expect_equal(
    template(0, 0, 150, NA, 0, hpc, hpc[,numeric()], hpc[,"compounds"]),
    eval_descrs(get_descr_form(compounds ~ 1, data = hpc))
  )
})

# ------------------------------------------------------------------------------

# Testing xy -> formula conversion

test_that("numeric y and dummy vars", {
  hpc2 <- dplyr::rename(hpc, ..y = class)
  rownames(hpc2) <- rownames(hpc2) # convert to char
  expect_equal(
    template(4, 4, 150, class_tab, 0, hpc2, hpc[, 1:4], hpc$class),
    eval_descrs(get_descr_xy(x = hpc[, 1:4], y = hpc$class))
  )

  hpc2 <- hpc[,c(4,5,1,2)]
  rownames(hpc2) <- rownames(hpc2)
  expect_equal(
    template(2, 2, 150, NA, 1, hpc2, hpc[,4:5], hpc[,1:2]),
    eval_descrs(get_descr_xy(x = hpc[, 4:5], y = hpc[, 1:2]))
  )

  hpc3 <- hpc2[,c("num_pending", "class", "compounds")]
  expect_equal(
    template(2, 2, 150, NA, 1, hpc3, hpc[, 4:5], hpc[, 1, drop = FALSE]),
    eval_descrs(get_descr_xy(x = hpc[, 4:5], y = hpc[, 1, drop = FALSE]))
  )
})

# ------------------------------------------------------------------------------

# Descriptor helpers

test_that("can be temporarily overriden at evaluation time", {

  scope_n_cols <- function() {
    scoped_descrs(list(.cols = function() { 1 }))
    .cols()
  }

  # .cols() overriden, but instantly reset
  expect_equal(
    scope_n_cols(),
    1
  )

  # .cols() should now be reset to an error
  expect_snapshot(error = TRUE, .cols())

})


# ------------------------------------------------------------------------------

test_that("system-level descriptor tests", {
  skip_if_not_installed("xgboost")
  skip_on_cran()

  expect_no_condition(
    boost_tree(mode = "regression", mtry = .cols()) |>
      set_engine("xgboost") |>
      fit_xy(x = mtcars[, -1], y = mtcars$mpg)
  )
  expect_no_condition(
    boost_tree(mode = "regression", mtry = .cols()) |>
      set_engine("xgboost") |>
      fit(mpg ~ ., data = mtcars)
  )

})
