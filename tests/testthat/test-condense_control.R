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

  ctrl$anotherone <- 2
  expect_snapshot(error = TRUE,
    condense_control(control_parsnip(), ctrl)
  )

  # Emulate being called from one of the upstream control_* functions
  control_test <- function(control = control_parsnip()) {
    control <- parsnip::condense_control(control_parsnip(), control)
    invisible(control)
  }
  expect_snapshot(error = TRUE, control_test(ctrl))
})
