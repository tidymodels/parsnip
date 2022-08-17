
test_that("primary argument", {

  normal <- survival_reg(dist = "lnorm")

  expect_snapshot(normal)

  expect_equal(
    normal$args,
    list(dist = rlang::quo("lnorm"))
  )

  dist_v <- survival_reg(dist = tune())

  expect_snapshot(dist_v)

  expect_equal(
    dist_v$args,
    list(dist = rlang::quo(tune()))
  )
})

test_that("updating", {
  expect_snapshot(
    survival_reg() %>%
      update(dist = "lnorm")
  )
})

test_that("bad input", {
  expect_error(survival_reg(mode = ", classification"))
})

test_that("wrong fit interface", {
  expect_error(
    expect_message(
      survival_reg() %>% fit_xy()
    ),
    "must use the formula interface"
  )
})
