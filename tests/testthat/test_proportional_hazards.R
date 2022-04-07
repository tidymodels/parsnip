test_that("printing", {
  expect_output(
    print(proportional_hazards()),
    "Proportional Hazards Model Specification \\(censored regression\\)"
  )
})

test_that("updating", {
  basic <- proportional_hazards()

  update_num <- update(basic, penalty = 0.05)
  expect_equal(
    update_num$args,
    list(penalty = quo(0.05),
         mixture = quo(NULL))
  )

  param_tibb <- tibble::tibble(penalty = 0.05)
  update_tibb <- update(basic, param_tibb)
  expect_equal(
    update_tibb$args,
    list(penalty = 0.05,
         mixture = quo(NULL))
  )

  param_list <- as.list(param_tibb)
  update_list <- update(basic, param_list)
  expect_equal(
    update_list$args,
    list(penalty = 0.05,
         mixture = quo(NULL))
  )
})


test_that("bad input", {
  expect_error(proportional_hazards(mode = ", classification"))
})

test_that("wrong fit interface", {
  expect_error(
    proportional_hazards() %>% fit_xy(),
    "must use the formula interface"
  )
})
