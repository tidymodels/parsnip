test_that("tunable parameters for gen_additive_mod + mgcv", {
  expect_snapshot({
    display_tunable_call_info(
      gen_additive_mod() |> set_engine("mgcv")
    )
  })
})
