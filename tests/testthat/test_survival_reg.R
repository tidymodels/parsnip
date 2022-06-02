
test_that("primary argument", {
  expect_message(
    normal <- survival_reg(dist = "lnorm")
  )
  expect_equal(
    normal$args,
    list(dist = "lnorm")
  )

  expect_message(
    dist_v <- survival_reg(dist = tune())
  )
  expect_equal(
    dist_v$args,
    list(dist = tune())
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
