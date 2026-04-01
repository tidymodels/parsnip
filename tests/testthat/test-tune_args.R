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

# tune_args.model_spec() --------------------------------------------------

test_that("tune_args() returns 0-row tibble when there are no tune() args", {
  res <- tune_args(linear_reg())
  expect_equal(nrow(res), 0)
})

test_that("tune_args() identifies tunable main args", {
  res <- tune_args(boost_tree(trees = tune(), learn_rate = tune("lr")))
  expect_equal(res$name, c("trees", "learn_rate"))
  expect_equal(res$id, c("trees", "lr"))
  expect_equal(res$source, c("model_spec", "model_spec"))
  expect_equal(res$component, c("boost_tree", "boost_tree"))
})

test_that("tune_args() identifies tunable engine args", {
  spec <- boost_tree() |> set_engine("xgboost", alpha = tune("a"))
  res <- tune_args(spec)
  expect_equal(res$name, "alpha")
  expect_equal(res$id, "a")
})

test_that("tune_args() uses arg name as id when no id is given", {
  res <- tune_args(boost_tree(trees = tune()))
  expect_equal(res$id, "trees")
})

test_that("tune_args() preserves unusual tune id", {
  res <- tune_args(boost_tree(trees = tune("odd name \n")))
  expect_equal(res$id, "odd name \n")
})

test_that("tune_args(full = TRUE) includes all tunable args regardless of tag", {
  res <- tune_args(boost_tree(trees = tune()), full = TRUE)
  expect_in("trees", res$name[res$tunable])
  expect_in("tree_depth", res$name[!res$tunable])
})
