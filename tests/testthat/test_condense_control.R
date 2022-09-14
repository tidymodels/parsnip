test_that("condense_control works", {
  ctrl <- control_parsnip()

  expect_equal(
    condense_control(ctrl, ctrl),
    ctrl
  )

  ctrl$allow_par <- TRUE
  ctrl$catch <- TRUE

  expect_equal(
    condense_control(ctrl, control_parsnip()),
    control_parsnip(catch = TRUE)
  )

  expect_snapshot(error = TRUE,
    condense_control(control_parsnip(), ctrl)
  )
})
