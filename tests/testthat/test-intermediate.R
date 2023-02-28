test_that("multiplication works", {

  tune_env <- rlang::new_environment()
  rlang::env_bind(tune_env, progress_env = rlang::new_environment())

  mockr::local_mock(
    get_tune_env = function() {tune_env},
    can_have_intermediate_result = function() {TRUE}
  )

  expect_false(has_intermediate_result("boopety_bop"))
  expect_null(set_intermediate_result("boopety_bop", 1L))
  expect_true(has_intermediate_result("boopety_bop"))
  expect_equal(get_intermediate_result("boopety_bop"), 1L)
  expect_equal(tune_env$progress_env$intermediate_results$boopety_bop[[1]], 1L)

  # can handle `NULL` results
  expect_false(has_intermediate_result("beepety_boop"))
  expect_null(set_intermediate_result("beepety_boop", NULL))
  expect_true(has_intermediate_result("beepety_boop"))
  expect_null(get_intermediate_result("beepety_boop"))
  expect_null(tune_env$progress_env$intermediate_results$beepety_boop[[1]])

  # previous intermediate result wasn't reset:
  expect_true(has_intermediate_result("boopety_bop"))
  expect_equal(get_intermediate_result("boopety_bop"), 1L)
})
