test_that("tunable parameters for survival_reg + flexsurv", {
  expect_snapshot({
    display_tunable_call_info(
      survival_reg() |> set_engine("flexsurv")
    )
  })
})

test_that("tunable parameters for survival_reg + survival", {
  expect_snapshot({
    display_tunable_call_info(
      survival_reg() |> set_engine("survival")
    )
  })
})

test_that("tunable parameters for survival_reg + flexsurvspline", {
  expect_snapshot({
    display_tunable_call_info(
      survival_reg() |> set_engine("flexsurvspline")
    )
  })
})
