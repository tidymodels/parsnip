library(dplyr)

# ------------------------------------------------------------------------------

context("attaching specifications")

# ------------------------------------------------------------------------------

test_that('regression models', {
  spec_env <- rlang::env()
  before_objs <- ls(pattern = "^reg_[a-zA-Z]", envir = spec_env)
  attach_parsnip_models(mode = "regression", env = spec_env)
  after_objs <- ls(pattern = "^reg_[a-zA-Z]", envir = spec_env)
  new_objs <- setdiff(after_objs, before_objs)
  expect_true(all(grepl("^reg_[a-zA-Z]", new_objs)))
  expect_true(sum(grepl("^reg_[a-zA-Z]", new_objs)) > 0)
})


test_that('classification models', {
  spec_env <- rlang::env()
  before_objs <- ls(pattern = "^cls_[a-zA-Z]", envir = spec_env)
  attach_parsnip_models(mode = "classification", env = spec_env)
  after_objs <- ls(pattern = "^cls_[a-zA-Z]", envir = spec_env)
  new_objs <- setdiff(after_objs, before_objs)
  expect_true(all(grepl("^cls_[a-zA-Z]", new_objs)))
  expect_true(sum(grepl("^cls_[a-zA-Z]", new_objs)) > 0)
})

test_that('all models', {
  spec_env <- rlang::env()
  pattern <- "(^cls_[a-zA-Z])|(^reg_[a-zA-Z])"
  before_objs <- ls(pattern = pattern, envir = spec_env)
  attach_parsnip_models(env = spec_env)
  after_objs <- ls(pattern = pattern, envir = spec_env)
  new_objs <- setdiff(after_objs, before_objs)
  expect_true(all(grepl(pattern, new_objs)))
  expect_true(sum(grepl(pattern, new_objs)) > 0)
})

test_that('filtering', {
  spec_env <- rlang::env()
  pattern <- "(multinom)|(linear)"
  before_objs <- ls(pattern = pattern, envir = spec_env)
  attach_parsnip_models(pattern, env = spec_env)
  after_objs <- ls(pattern = pattern, envir = spec_env)
  new_objs <- setdiff(after_objs, before_objs)
  expect_true(all(grepl(pattern, new_objs)))
  expect_true(sum(grepl(pattern, new_objs)) > 0)
})

