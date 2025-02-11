test_that("warns informatively about protected arguments", {
  obj <- list(protect = c("a", "b"))
  core_args <- c("c", "d")

  expect_snapshot(
    .res <- check_eng_args(
      args = list(a = 1, b = 2, c = 3, e = 5),
      obj = obj,
      core_args = core_args
    )
  )

  expect_named(.res, "e")

  expect_snapshot(
    .res <- check_eng_args(
      args = list(b = 2, c = 3, e = 5),
      obj = obj,
      core_args = core_args
    )
  )

  expect_named(.res, "e")

  expect_snapshot(
    .res <- check_eng_args(
      args = list(c = 3, e = 5),
      obj = obj,
      core_args = core_args
    )
  )

  expect_named(.res, "e")

  expect_warning(
    check_eng_args(
      args = list(a = 1, b = 2, c = 3, e = 5),
      obj = obj,
      core_args = core_args
    ),
    class = "parsnip_protected_arg_warning"
  )
})
