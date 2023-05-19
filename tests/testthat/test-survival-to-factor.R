test_that('convert survival data to factor', {

  surv_obj <-
    structure(
      c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1),
      dim = c(10L, 2L),
      dimnames = list(NULL, c("time", "status")),
      type = "right",
      class = "Surv")

  lvls <- c("event", "non-event")
  to_factor <- function(x) factor(x, levels = lvls)

  obs_time_1.5 <- .time_as_binary_event(surv_obj, 1.5)
  exp_time_1.5 <- to_factor(c(NA, rep("non-event", 9)))
  expect_equal(obs_time_1.5, exp_time_1.5)

  obs_time_5.5 <- .time_as_binary_event(surv_obj, 5.5)
  exp_time_5.5 <- to_factor(c(rep(c(NA, "event"), 2), NA, rep("non-event", 5)))
  expect_equal(obs_time_5.5, exp_time_5.5)

  obs_time_11 <- .time_as_binary_event(surv_obj, 11)
  exp_time_11 <- to_factor(rep(c(NA, "event"), 5))
  expect_equal(obs_time_11, exp_time_11)

  expect_snapshot_error(.time_as_binary_event(surv_obj, 11:12))
  expect_snapshot_error(.time_as_binary_event(surv_obj, Inf))
  expect_snapshot_error(.time_as_binary_event(surv_obj, NA))
  expect_snapshot_error(.time_as_binary_event(surv_obj, -1))
  expect_snapshot_error(.time_as_binary_event(surv_obj, "potato"))
})
