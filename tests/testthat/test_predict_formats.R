hpc <- hpc_data[1:150, c(2:5, 8)]

# ------------------------------------------------------------------------------


lm_fit <-
  linear_reg(mode = "regression") %>%
  set_engine("lm") %>%
  fit(compounds ~ ., data = hpc)

class_dat <- airquality[complete.cases(airquality),]
class_dat$Ozone <- factor(ifelse(class_dat$Ozone >= 31, "high", "low"))

lr_fit <-
  logistic_reg() %>%
  set_engine("glm") %>%
  fit(Ozone ~ ., data = class_dat)

class_dat2 <- airquality[complete.cases(airquality),]
class_dat2$Ozone <- factor(ifelse(class_dat2$Ozone >= 31, "high+values", "2low"))

lr_fit_2 <-
  logistic_reg() %>%
  set_engine("glm") %>%
  fit(Ozone ~ ., data = class_dat2)

# ------------------------------------------------------------------------------

test_that('regression predictions', {
  expect_true(is_tibble(predict(lm_fit, new_data = hpc[1:5,-1])))
  expect_true(is.vector(parsnip:::predict_numeric.model_fit(lm_fit, new_data = hpc[1:5,-1])))
  expect_equal(names(predict(lm_fit, new_data = hpc[1:5,-1])), ".pred")
})

test_that('classification predictions', {
  expect_true(is_tibble(predict(lr_fit, new_data = class_dat[1:5,-1])))
  expect_true(is.factor(parsnip:::predict_class.model_fit(lr_fit, new_data = class_dat[1:5,-1])))
  expect_equal(names(predict(lr_fit, new_data = class_dat[1:5,-1])), ".pred_class")

  expect_true(is_tibble(predict(lr_fit, new_data = class_dat[1:5,-1], type = "prob")))
  expect_true(is_tibble(parsnip:::predict_classprob.model_fit(lr_fit, new_data = class_dat[1:5,-1])))
  expect_equal(names(predict(lr_fit, new_data = class_dat[1:5,-1], type = "prob")),
               c(".pred_high", ".pred_low"))
})

test_that('non-standard levels', {
  expect_true(is_tibble(predict(lr_fit, new_data = class_dat[1:5,-1])))
  expect_true(is.factor(parsnip:::predict_class.model_fit(lr_fit, new_data = class_dat[1:5,-1])))
  expect_equal(names(predict(lr_fit, new_data = class_dat[1:5,-1])), ".pred_class")

  expect_true(is_tibble(predict(lr_fit_2, new_data = class_dat2[1:5,-1], type = "prob")))
  expect_true(is_tibble(parsnip:::predict_classprob.model_fit(lr_fit_2, new_data = class_dat2[1:5,-1])))
  expect_equal(names(predict(lr_fit_2, new_data = class_dat2[1:5,-1], type = "prob")),
               c(".pred_2low", ".pred_high+values"))
  expect_equal(names(parsnip:::predict_classprob.model_fit(lr_fit_2, new_data = class_dat2[1:5,-1])),
               c("2low", "high+values"))
})

test_that('predict(type = "prob") with level "class" (see #720)', {
  x <- tibble::tibble(
    boop = factor(sample(c("class", "class_1"), 100, replace = TRUE)),
    bop = rnorm(100),
    beep = rnorm(100)
  )

  expect_error(
    regexp = NA,
    mod <- logistic_reg() %>%
      set_mode(mode = "classification") %>%
      fit(boop ~ bop + beep, data = x)
  )

  expect_error(
    regexp = NA,
    predict(mod, type = "class", new_data = x)
  )

  expect_error(
    regexp = "variable `boop` has a level called 'class'",
    predict(mod, type = "prob", new_data = x)
  )
})


test_that('non-factor classification', {
  skip_if(run_glmnet)

  expect_error(
    logistic_reg() %>%
      set_engine("glm") %>%
      fit(class ~ .,
          data = hpc %>% dplyr::mutate(class = class == "VF"))
  )
  expect_error(
    logistic_reg() %>%
      set_engine("glm") %>%
      fit(class ~ .,
          data = hpc %>% dplyr::mutate(class = ifelse(class == "VF", 1, 0)))
  )

  expect_error(
    multinom_reg() %>%
      set_engine("glmnet") %>%
      fit(class ~ .,
          data = hpc %>% dplyr::mutate(class = as.character(class)))
  )
})


