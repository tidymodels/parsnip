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

# find_tune_id() ----------------------------------------------------------

test_that("find_tune_id() finds tune() nested in expressions", {
  expect_identical(find_tune_id(quote(log(tune()))), "")
  expect_identical(find_tune_id(quote(log(tune("nested")))), "nested")
})

test_that("find_tune_id() returns NA for empty input", {
  expect_identical(find_tune_id(list()), NA_character_)
})

test_that("find_tune_id() errors with multiple tune() in one arg", {
  expect_snapshot(
    error = TRUE,
    find_tune_id(quote(c(tune(), tune())))
  )
})
