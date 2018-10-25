library(testthat)
library(parsnip)

# ------------------------------------------------------------------------------

context("descriptor variables")

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

species_tab <- table(iris$Species, dnn = NULL)

# ------------------------------------------------------------------------------

context("Should descriptors be created?")

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
  expect_false(parsnip:::requires_descrs(rand_forest(mtry = varying())))
  expect_true(parsnip:::requires_descrs(rand_forest(mtry = .cols())))
  expect_false(parsnip:::requires_descrs(rand_forest(mtry = expr(3))))
  expect_false(parsnip:::requires_descrs(rand_forest(mtry = quote(3))))
  expect_true(parsnip:::requires_descrs(rand_forest(mtry = fn())))
  expect_true(parsnip:::requires_descrs(rand_forest(mtry = fn2())))

  # descriptors in `eng_args`
  expect_false(parsnip:::requires_descrs(rand_forest() %>% set_engine("ranger", arrrg = 3)))
  expect_false(parsnip:::requires_descrs(rand_forest() %>% set_engine("ranger", arrrg = varying())))
  expect_true(parsnip:::requires_descrs(rand_forest() %>% set_engine("ranger", arrrg = .obs())))
  expect_false(parsnip:::requires_descrs(rand_forest() %>% set_engine("ranger", arrrg = expr(3))))
  expect_true(parsnip:::requires_descrs(rand_forest() %>% set_engine("ranger", arrrg = fn())))
  expect_true(parsnip:::requires_descrs(rand_forest() %>% set_engine("ranger", arrrg = fn2())))

  # mixed
  expect_true(
    parsnip:::requires_descrs(
      rand_forest(mtry = 3) %>% set_engine("ranger", arrrg = fn2())
    )
  )

  expect_true(
    parsnip:::requires_descrs(
      rand_forest(mtry = .cols()) %>% set_engine("ranger", arrrg = 3)
    )
  )
})


# ------------------------------------------------------------------------------

context("Testing formula -> xy conversion")

test_that("numeric y and dummy vars", {
  expect_equal(
    template(5, 4, 150, NA, 1, iris, iris[-2], iris[,"Sepal.Width"]),
    eval_descrs(get_descr_form(Sepal.Width ~ ., data = iris))
  )
  expect_equal(
    template(2, 1, 150, NA, 1, iris, iris["Species"], iris[,"Sepal.Width"]),
    eval_descrs(get_descr_form(Sepal.Width ~ Species, data = iris))
  )
})

test_that("numeric y and x", {
  expect_equal(
    template(1, 1, 150, NA, 0, iris, iris["Sepal.Length"], iris[,"Sepal.Width"]),
    eval_descrs(get_descr_form(Sepal.Width ~ Sepal.Length, data = iris))
  )
  expect_equal(
    {
      log_sep <- iris["Sepal.Length"]
      log_sep[["Sepal.Length"]] <- log(log_sep[["Sepal.Length"]])
      names(log_sep) <- "log(Sepal.Length)"
      template(1, 1, 150, NA, 0, iris, log_sep, iris[,"Sepal.Width"])
    },
    eval_descrs(get_descr_form(Sepal.Width ~ log(Sepal.Length), data = iris))
  )
})

test_that("factor y", {
  expect_equal(
    template(4, 4, 150, species_tab, 0, iris, iris[-5], iris[,"Species"]),
    eval_descrs(get_descr_form(Species ~ ., data = iris))
  )
  expect_equal(
    template(1, 1, 150, species_tab, 0, iris, iris["Sepal.Length"], iris[,"Species"]),
    eval_descrs(get_descr_form(Species ~ Sepal.Length, data = iris))
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
  # So model.frame ignores - signs in a model formula so Species is not removed
  # prior to model.matrix; otherwise this should have n_cols = 3
  expect_equal(
    template(3, 4, 150, NA, 1, iris, iris[-2], iris[,"Sepal.Width"]),
    eval_descrs(get_descr_form(Sepal.Width ~ . - Species, data = iris))
  )

  # Oy ve! Before going to model.matrix, model.frame produces a data frame
  # with one column and that column is a matrix (with the results from
  # `poly(Sepal.Length, 3)`
  x <- model.frame(~poly(Sepal.Length, 3), iris)
  attributes(x) <- attributes(as.data.frame(x))[c("names", "class", "row.names")]
  expect_equal(
    template(3, 1, 150, NA, 0, iris, x, iris[,"Sepal.Width"]),
    eval_descrs(get_descr_form(Sepal.Width ~ poly(Sepal.Length, 3), data = iris))
  )

  expect_equal(
    template(0, 0, 150, NA, 0, iris, iris[,numeric()], iris[,"Sepal.Width"]),
    eval_descrs(get_descr_form(Sepal.Width ~ 1, data = iris))
  )
})

# ------------------------------------------------------------------------------

context("Testing xy -> formula conversion")

test_that("numeric y and dummy vars", {
  iris2 <- dplyr::rename(iris, ..y = Species)
  rownames(iris2) <- rownames(iris2) # convert to char
  expect_equal(
    template(4, 4, 150, species_tab, 0, iris2, iris[, 1:4], iris$Species),
    eval_descrs(get_descr_xy(x = iris[, 1:4], y = iris$Species))
  )

  iris2 <- iris[,c(4,5,1,2)]
  rownames(iris2) <- rownames(iris2)
  expect_equal(
    template(2, 2, 150, NA, 1, iris2, iris[,4:5], iris[,1:2]),
    eval_descrs(get_descr_xy(x = iris[, 4:5], y = iris[, 1:2]))
  )

  iris3 <- iris2[,c("Petal.Width", "Species", "Sepal.Length")]
  expect_equal(
    template(2, 2, 150, NA, 1, iris3, iris[, 4:5], iris[, 1, drop = FALSE]),
    eval_descrs(get_descr_xy(x = iris[, 4:5], y = iris[, 1, drop = FALSE]))
  )
})

# ------------------------------------------------------------------------------

context("spark descriptors")

test_that("spark descriptor", {

  skip_if_not_installed("sparklyr")

  library(sparklyr)
  library(dplyr)

  sc <- try(spark_connect(master = "local"), silent = TRUE)

  skip_if(inherits(sc, "try-error"))

  npk_descr  <- copy_to(sc,  npk[, 1:4],  "npk_descr", overwrite = TRUE)
  iris_descr <- copy_to(sc,        iris, "iris_descr", overwrite = TRUE)

  # spark does not allow .x, .y, .dat
  template2 <- purrr::partial(template, x = NULL, y = NULL, dat = NULL)
  eval_descrs2 <- purrr::partial(eval_descrs, not = c(".x", ".y", ".dat"))

  expect_equal(
    template2(5, 4, 150, NA, 1),
    eval_descrs2(get_descr_form(Sepal_Width ~ ., data = iris_descr))
  )
  expect_equal(
    template2(2, 1, 150, NA, 1),
    eval_descrs2(get_descr_form(Sepal_Width ~ Species, data = iris_descr))
  )
  expect_equal(
    template2(1, 1, 150, NA, 0),
    eval_descrs2(get_descr_form(Sepal_Width ~ Sepal_Length, data = iris_descr))
  )
  expect_equivalent(
    template2(4, 4, 150, species_tab, 0),
    eval_descrs2(get_descr_form(Species ~ ., data = iris_descr))
  )
  expect_equal(
    template2(1, 1, 150, species_tab, 0),
    eval_descrs2(get_descr_form(Species ~ Sepal_Length, data = iris_descr))
  )
  expect_equivalent(
    template2(7, 3, 24, rev(table(npk$K, dnn = NULL)), 3),
    eval_descrs2(get_descr_form(K ~ ., data = npk_descr))
  )

})

# ------------------------------------------------------------------------------

context("Descriptor helpers")

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
  expect_error(.cols())

})


