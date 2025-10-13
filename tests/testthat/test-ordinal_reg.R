test_that("testing", {
  # Testing is done in {ordered}
  # https://github.com/tidymodels/ordered

  expect_true(TRUE)
})

test_that("odds_link", {
  # a legitimate odds link function not recognized by `polr()`
  tidy_spec <- ordinal_reg(engine = "polr", odds_link = "adjacent_categories")
  expect_warning(translate(tidy_spec), "odds_link")
})
