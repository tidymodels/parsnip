# new_ordinal_translation() is in helper-ordinal-reg.R

test_that("testing", {
  # Testing is done in {ordered}
  # https://github.com/corybrunson/ordered

  expect_true(TRUE)
})

test_that("odds_link", {
  # a legitimate odds link function not recognized by {dials}
  tidy_spec <- ordinal_reg(engine = "polr", odds_link = "adjacent_categories")
  expect_snapshot(error = TRUE, {
    check_args(tidy_spec)
  })
})

test_that("parallel_reg is validated", {
  expect_no_error(check_args(ordinal_reg(parallel_reg = NULL)))
  expect_no_error(check_args(ordinal_reg(parallel_reg = TRUE)))
  expect_no_error(check_args(ordinal_reg(parallel_reg = FALSE, engine = "clm")))

  expect_snapshot(error = TRUE, {
    check_args(ordinal_reg(parallel_reg = NA))
  })
  expect_snapshot(error = TRUE, {
    check_args(ordinal_reg(parallel_reg = c(TRUE, FALSE)))
  })
  expect_snapshot(error = TRUE, {
    check_args(ordinal_reg(parallel_reg = 1))
  })
})

test_that("parallel_reg cannot be combined with a nominal engine argument", {
  expect_snapshot(error = TRUE, {
    ordinal_reg(parallel_reg = FALSE) |>
      set_engine("clm", nominal = ~x) |>
      check_args()
  })

  expect_no_error(
    ordinal_reg(parallel_reg = NULL) |>
      set_engine("clm", nominal = ~x) |>
      check_args(spec)
  )
})

test_that("ordinalNet penalty path is built during translation", {
  # value translation (link/family/parallel_reg) is handled in {ordered}'s
  # `ordinalNet_wrapper()`; parsnip only constructs the penalty path here
  x <- new_ordinal_translation(
    list(
      link = rlang::quo("probit"),
      family = rlang::quo("stopping_ratio"),
      parallel_reg = rlang::quo(FALSE),
      lambdaVals = 0.2
    ),
    penalty = 0.1
  )

  result <- translate_ordinal_reg_ordinalNet(x)

  expect_equal(result$method$fit$args$lambdaMinRatio, 0.2)
  expect_equal(result$method$fit$args$nLambda, 120L)
  expect_true(result$method$fit$args$includeLambda0)
  expect_null(result$method$fit$args$lambdaVals)
})

test_that("unsupported non-parallel models give engine guidance", {
  spec <- ordinal_reg(parallel_reg = FALSE)

  expect_snapshot(error = TRUE, {
    check_ordinal_reg_parallel(spec, "polr")
  })
  expect_no_error(check_ordinal_reg_parallel(spec, "ordinalNet"))
})
