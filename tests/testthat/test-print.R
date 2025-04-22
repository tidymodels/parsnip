test_that("model spec print methods work (whole game)", {
  expect_snapshot(svm_poly())
  expect_snapshot(boost_tree(mtry = 5))
  expect_snapshot(rand_forest() |> set_mode("regression"))
  expect_snapshot(logistic_reg() |> set_engine("glmnet", penalty = .5))
  expect_snapshot(mlp() |> set_mode("classification") |> translate())
})

test_that("`print_model_spec()` handles args correctly", {
  lr <- linear_reg()
  class(lr)[1] <- "beep"

  expect_snapshot(print_model_spec(linear_reg()))
  expect_snapshot(print_model_spec(lr))
  expect_snapshot(print_model_spec(lr, cls = "boop"))
  expect_snapshot(print_model_spec(lr, cls = "boop", desc = "Boop"))
})

test_that("`get_model_desc()` retrieves/creates model description well", {
  expect_equal(get_model_desc("linear_reg"), "Linear Regression")
  expect_equal(get_model_desc("boost_tree"), "Boosted Tree")
  expect_equal(get_model_desc("boost_tree"),
               model_descs$desc[model_descs$cls == "boost_tree"])

  expect_equal(get_model_desc("goofy new class"), "goofy new class")
  expect_equal(get_model_desc("goofy_new_class"), "goofy new class")
  expect_equal(get_model_desc("goofy.new_class"), "goofy new class")
  expect_equal(get_model_desc("goofy.new.class"), "goofy new class")
})
