# new_ordinal_translation() is in helper-ordinal-reg.R

test_that("testing", {
  # Testing is done in {ordered}
  # https://github.com/tidymodels/ordered

  expect_true(TRUE)
})

test_that("odds_link", {
  # a legitimate odds link function not recognized by {dials}
  tidy_spec <- ordinal_reg(engine = "polr", odds_link = "adjacent_categories")
  expect_snapshot(. <- translate(tidy_spec))
})

test_that("parallel_reg is validated", {
  expect_no_error(check_args(ordinal_reg(parallel_reg = NULL)))
  expect_no_error(check_args(ordinal_reg(parallel_reg = TRUE)))
  expect_no_error(check_args(ordinal_reg(parallel_reg = FALSE)))

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

test_that("clm arguments are translated", {
  x <- new_ordinal_translation(
    list(
      link = rlang::quo("logistic"),
      threshold = rlang::quo("symmetric_zero"),
      parallel_reg = rlang::quo(FALSE)
    )
  )

  result <- translate_ordinal_reg_clm(x)

  expect_equal(result$method$fit$args$link, "logit")
  expect_equal(result$method$fit$args$threshold, "symmetric2")
  expect_null(result$method$fit$args$parallel_reg)

  formula <- y ~ `not valid` + log(x)
  nominal <- rlang::eval_tidy(result$method$fit$args$nominal)
  expect_equal(
    rlang::f_rhs(nominal),
    rlang::expr(`not valid` + x)
  )
  expect_identical(rlang::f_env(nominal), rlang::f_env(formula))

  x$method$fit$args$parallel_reg <- rlang::quo(TRUE)
  x$method$fit$args$nominal <- rlang::quo(unused)
  result <- translate_ordinal_reg_clm(x)
  expect_null(result$method$fit$args$nominal)
})

test_that("VGAM arguments are translated", {
  x <- new_ordinal_translation(
    list(
      link = rlang::quo("logistic"),
      family = rlang::quo("continuation_ratio"),
      Thresh = rlang::quo("equidistant"),
      untouched = 1
    )
  )

  result <- translate_ordinal_reg_vglm(x)

  expect_equal(result$method$fit$args$link, "logitlink")
  expect_equal(result$method$fit$args$family, "cratio")
  expect_equal(result$method$fit$args$Thresh, "equid")
  expect_equal(result$method$fit$args$untouched, 1)

  expect_snapshot(error = TRUE, {
    match_ordinal_link_vglm("loglog")
  })
  expect_snapshot(error = TRUE, {
    translate_ordinal_vgam_args(
      list(
        link = rlang::quo("logistic"),
        family = rlang::quo("adjacent_categories"),
        Thresh = NULL
      )
    )
  })
})

test_that("parameter values are matched against the model argument", {
  expect_snapshot(error = TRUE, {
    match_ordinal_link_vglm("logisitc")
  })
  # no partial matching
  expect_snapshot(error = TRUE, {
    match_ordinal_family("cumu")
  })
  expect_snapshot(error = TRUE, {
    match_threshold_structure_vglm(c("flexible", "equidistant"))
  })
})

test_that("ordinalNet arguments are translated", {
  x <- new_ordinal_translation(
    list(
      link = rlang::quo("probit"),
      family = rlang::quo("stopping_ratio"),
      parallel_reg = rlang::quo(FALSE),
      lambdaVals = 0.2
    )
  )

  result <- translate_ordinal_reg_ordinalNet(x)

  expect_equal(result$method$fit$args$link, "probit")
  expect_equal(result$method$fit$args$family, "sratio")
  expect_false(result$method$fit$args$parallelTerms)
  expect_true(result$method$fit$args$nonparallelTerms)
  expect_null(result$method$fit$args$parallel_reg)
  expect_equal(result$method$fit$args$lambdaMinRatio, 0.2)
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
