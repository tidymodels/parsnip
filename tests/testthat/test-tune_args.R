# tune_id() ---------------------------------------------------------------

test_that("tune_id() extracts id from tune() calls", {
  expect_identical(tune_id(quote(tune("my_id"))), "my_id")
  expect_identical(tune_id(quote(tune())), "")
})

test_that("tune_id() returns NA for non-tune input", {
  expect_identical(tune_id(NULL), NA_character_)
  expect_identical(tune_id(quote(log(2))), NA_character_)
  expect_identical(tune_id(1), NA_character_)
  expect_identical(tune_id("a"), NA_character_)
  expect_identical(tune_id(rlang::quos(NULL)), NA_character_)
})
