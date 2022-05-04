

test_that('fit ctree models', {
  skip_if_not_installed("modeldata")
  skip_if_not_installed("partykit")
  skip_on_cran()

  data(Chicago, package = "modeldata")
  data(ad_data, package = "modeldata")

  expect_error(
    fit_1 <- ctree_train(ridership ~ ., data = Chicago[, 1:20]),
    regex = NA
  )
  expect_error(
    fit_2 <- ctree_train(ridership ~ ., data = Chicago[, 1:20],
                         mincriterion = 1/2, maxdepth = 2),
    regex = NA
  )
  expect_equal(fit_2$info$control$logmincriterion, log(1/2))
  expect_equal(fit_2$info$control$maxdepth, 2)
  expect_error(
    fit_3 <- ctree_train(ridership ~ ., data = Chicago[, 1:20],
                         mincriterion = 1/2, maxdepth = 2,
                         weights = 1:nrow(Chicago)),
    regex = NA
  )
  expect_false(isTRUE(all.equal(fit_2$fitted, fit_3$fitted)))
  expect_error(
    fit_4 <- ctree_train(Class ~ ., data = ad_data),
    regex = NA
  )
  expect_snapshot_error(
    ctree_train(ridership ~ ., data = Chicago[, 1:20],
                mincriterion = 1/2, maxdepth = 2,
                weights = runif(nrow(Chicago)))
  )
})

test_that('fit cforest models', {
  skip_if_not_installed("modeldata")
  skip_if_not_installed("partykit")
  skip_on_cran()

  data(Chicago, package = "modeldata")
  data(ad_data, package = "modeldata")

  expect_error(
    fit_1 <- cforest_train(ridership ~ ., data = Chicago[, 1:5], ntree = 2),
    regex = NA
  )
  expect_equal(length(fit_1$nodes), 2)
  expect_error(
    fit_2 <- cforest_train(ridership ~ ., data = Chicago[, 1:5], ntree = 2,
                           mincriterion = 1/2, maxdepth = 2, mtry = 4),
    regex = NA
  )
  expect_equal(fit_2$info$control$logmincriterion, log(1/2))
  expect_equal(fit_2$info$control$maxdepth, 2)
  expect_equal(fit_2$info$control$mtry, 4)
  expect_error(
    fit_3 <- cforest_train(ridership ~ ., data = Chicago[, 1:5], ntree = 2,
                           mincriterion = 1/2, maxdepth = 2, mtry = 4,
                           weights = 1:nrow(Chicago)),
    regex = NA
  )
  expect_false(isTRUE(all.equal(fit_2$fitted, fit_3$fitted)))
  expect_error(
    fit_4 <- cforest_train(Class ~ ., data = ad_data, ntree = 2),
    regex = NA
  )
  expect_error(
    fit_5 <- cforest_train(Class ~ ., data = ad_data, ntree = 2, mtry = 2000),
    regex = NA
  )
  expect_equal(fit_5$info$control$mtry, 130)
  expect_snapshot_error(
    cforest_train(ridership ~ ., data = Chicago[, 1:20], weights = "potato")
  )
})
