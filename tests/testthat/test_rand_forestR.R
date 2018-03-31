library(testthat)
library(parsnip)
library(recipes)

test_that('primary arguments', {
  mtry <- rand_forest(mode = "regression", mtry = 4)
  mtry_ranger <- translate(mtry, engine = "ranger")
  mtry_randomForest <- translate(mtry, engine = "randomForest")
  expect_equal(mtry_ranger$method$fit_call,
               quote(
                 ranger(
                   formula = missing_arg(),
                   data = missing_arg(),
                   mtry = 4
                 )
               )
  )
  expect_equal(mtry_randomForest$method$fit_call,
               quote(
                 randomForest(
                   x =  missing_arg(),
                   y =  missing_arg(),
                   mtry = 4
                 )
               )
  )  
  
  trees <- rand_forest(mode = "classification", trees = 1000)
  trees_ranger <- translate(trees, engine = "ranger")
  trees_randomForest <- translate(trees, engine = "randomForest")
  expect_equal(trees_ranger$method$fit_call,
               quote(
                 ranger(
                   formula = missing_arg(),
                   data = missing_arg(),
                   num.trees = 1000
                 )
               )
  )
  expect_equal(trees_randomForest$method$fit_call,
               quote(
                 randomForest(
                   x = missing_arg(),
                   y = missing_arg(),
                   ntree = 1000
                 )
               )
  )    
  
  min_n <- rand_forest(mode = "regression", min_n = 5)
  min_n_ranger <- translate(min_n, engine = "ranger")
  min_n_randomForest <- translate(min_n, engine = "randomForest")
  expect_equal(min_n_ranger$method$fit_call,
               quote(
                 ranger(
                   formula = missing_arg(),
                   data = missing_arg(),
                   min.node.size = 5
                 )
               )
  )
  expect_equal(min_n_randomForest$method$fit_call,
               quote(
                 randomForest(
                   x = missing_arg(),
                   y = missing_arg(),
                   nodesize = 5
                 )
               )
  )      
  
  mtry_v <- rand_forest(mode = "classification", mtry = varying())
  mtry_v_ranger <- translate(mtry_v, engine = "ranger")
  mtry_v_randomForest <- translate(mtry_v, engine = "randomForest")
  expect_equal(mtry_v_ranger$method$fit_call,
               quote(
                 ranger(
                   formula = missing_arg(),
                   data = missing_arg(),
                   mtry = varying()
                 )
               )
  )
  expect_equal(mtry_v_randomForest$method$fit_call,
               quote(
                 randomForest(
                   x = missing_arg(),
                   y = missing_arg(),
                   mtry = varying()
                 )
               )
  )  
  
  trees_v <- rand_forest(mode = "regression", trees = varying())
  trees_v_ranger <- translate(trees_v, engine = "ranger")
  trees_v_randomForest <- translate(trees_v, engine = "randomForest")
  expect_equal(trees_v_ranger$method$fit_call,
               quote(
                 ranger(
                   formula = missing_arg(),
                   data = missing_arg(),
                   num.trees = varying()
                 )
               )
  )
  expect_equal(trees_v_randomForest$method$fit_call,
               quote(
                 randomForest(
                   x = missing_arg(),
                   y = missing_arg(),
                   ntree = varying()
                 )
               )
  )    
  
  min_n_v <- rand_forest(mode = "classification", min_n = varying())
  min_n_v_ranger <- translate(min_n_v, engine = "ranger")
  min_n_v_randomForest <- translate(min_n_v, engine = "randomForest")
  expect_equal(min_n_v_ranger$method$fit_call,
               quote(
                 ranger(
                   formula = missing_arg(),
                   data = missing_arg(),
                   min.node.size = varying()
                 )
               )
  )
  expect_equal(min_n_v_randomForest$method$fit_call,
               quote(
                 randomForest(
                   x = missing_arg(),
                   y = missing_arg(),
                   nodesize = varying()
                 )
               )
  )        
})

test_that('engine arguments', {
  ranger_imp <- rand_forest(mode = "classification", others = list(importance = "impurity"))
  expect_equal(translate(ranger_imp, engine = "ranger")$method$fit_call,
               quote(
                 ranger(
                   formula = missing_arg(),
                   data = missing_arg(),
                   importance = "impurity"
                 )
               )
  )
  
  randomForest_votes <- rand_forest(mode = "regression", others = list(norm.votes = FALSE))
  expect_equal(translate(randomForest_votes, engine = "randomForest")$method$fit_call,
               quote(
                 randomForest(
                   x = missing_arg(),
                   y = missing_arg(),
                   norm.votes = FALSE
                 )
               )
  ) 
  
  
  ranger_samp_frac <- rand_forest(mode = "regression", others = list(sample.fraction = varying()))
  expect_equal(translate(ranger_samp_frac, engine = "ranger")$method$fit_call,
               quote(
                 ranger(
                   formula = missing_arg(),
                   data = missing_arg(),
                   sample.fraction = varying()
                 )
               )
  ) 
  

  randomForest_votes_v <- rand_forest(mode = "regression", others = list(norm.votes = FALSE, sampsize = varying()))
  expect_equal(translate(randomForest_votes_v, engine = "randomForest")$method$fit_call,
               quote(
                 randomForest(
                   x = missing_arg(),
                   y = missing_arg(),
                   sampsize = varying(),
                   norm.votes = FALSE
                 )
               )
  )   
})


test_that('updating', {
  expr1     <- rand_forest(mode = "regression",           others = list(norm.votes = FALSE, sampsize = varying()))
  expr1_exp <- rand_forest(mode = "regression", mtry = 2, others = list(norm.votes = FALSE, sampsize = varying()))
  
  expr2     <- rand_forest(mode = "regression", mtry = 7, min_n = varying())
  expr2_exp <- rand_forest(mode = "regression", mtry = 7, min_n = varying(), others = list(norm.votes = FALSE))
  
  expr3     <- rand_forest(mode = "regression", mtry = 7, min_n = varying())
  expr3_exp <- rand_forest(mode = "regression", mtry = 2)
  
  expr4     <- rand_forest(mode = "regression", mtry = 2, others = list(norm.votes = FALSE, sampsize = varying()))
  expr4_exp <- rand_forest(mode = "regression", mtry = 2, others = list(norm.votes = TRUE, sampsize = varying()))
  
  expr5     <- rand_forest(mode = "regression", mtry = 2, others = list(norm.votes = FALSE))
  expr5_exp <- rand_forest(mode = "regression", mtry = 2, others = list(norm.votes = TRUE, sampsize = varying()))
  
  expect_equal(update(expr1, mtry = 2), expr1_exp)
  expect_equal(update(expr2, others = list(norm.votes = FALSE)), expr2_exp)  
  expect_equal(update(expr3, mtry = 2, fresh = TRUE), expr3_exp)  
  expect_equal(update(expr4, others = list(norm.votes = TRUE)), expr4_exp)
  expect_equal(update(expr5, others = list(norm.votes = TRUE, sampsize = varying())), expr5_exp)
  
})

test_that('bad input', {
  expect_error(rand_forest(mode = "classification", case.weights = var))
  expect_error(rand_forest(mode = "time series"))
  expect_error(translate(rand_forest(mode = "classification"), engine = "wat?"))
  expect_warning(translate(rand_forest(mode = "classification"), engine = NULL))  
  expect_error(translate(rand_forest(mode = "classification", others = list(ytest = 2))))
  expect_error(translate(rand_forest(mode = "regression", formula = y ~ x)))
  expect_error(translate(rand_forest(mode = "classification", others = list(x = x, y = y)), engine = "randomForest"))
  expect_error(translate(rand_forest(mode = "regression", others = list(formula = y ~ x)), engine = ""))
})


###################################################################

data("lending_club")
lending_club <- head(lending_club, 200)
lc_form <- as.formula(Class ~ funded_amnt + term)
num_pred <- c("funded_amnt", "annual_inc", "num_il_tl")
lc_bad_form <- as.formula(funded_amnt ~ term)

lc_basic <- rand_forest(mode = "classification")
lc_ranger <- rand_forest(mode = "classification", others = list(seed = 144))


bad_ranger_cls <- rand_forest(mode = "classification", 
                              others = list(min.node.size = -10))
bad_rf_cls <- rand_forest(mode = "classification", 
                          others = list(sampsize = -10))

ctrl <- list(verbosity = 1, catch = FALSE)
caught_ctrl <- list(verbosity = 1, catch = TRUE)
quiet_ctrl <- list(verbosity = 0, catch = TRUE)

lc_rec <- recipe(Class ~ funded_amnt + annual_inc+ num_il_tl, 
                 data = lending_club)
bad_lc_rec <- recipe(total_bal_il ~ funded_amnt + annual_inc+ num_il_tl, 
                     data = lending_club)


test_that('ranger execution', {
  skip_on_cran()
  
  expect_error(
    res <- fit(
      lc_ranger,
      engine = "ranger",
      .control = ctrl,
      lc_form,
      data = lending_club
    ),
    regexp = NA
  )
  expect_error(
    res <- fit(
      lc_ranger,
      engine = "ranger",
      .control = ctrl,
      x = lending_club[, num_pred],
      y = lending_club$Class
    ),
    regexp = NA
  )
  expect_error(
    res <- fit(
      lc_ranger,
      engine = "ranger",
      .control = ctrl,
      lc_rec,
      data = lending_club
    ),
    regexp = NA
  )
  
  expect_error(
    res <- fit(
      bad_ranger_cls,
      engine = "ranger",
      .control = ctrl,
      lc_bad_form,
      data = lending_club
    )
  )
  
  ranger_form_catch <- fit(
    bad_ranger_cls,
    engine = "ranger",
    .control = caught_ctrl,
    lc_bad_form,
    data = lending_club
  )
  expect_true(inherits(ranger_form_catch, "try-error")) 
  
  # fails
  # ranger_xy_catch <- fit(
  #   bad_ranger_cls,
  #   engine = "ranger",
  #   .control = caught_ctrl,
  #   x = lending_club[, num_pred],
  #   y = lending_club$total_bal_il
  # )
  # expect_true(inherits(ranger_xy_catch, "try-error"))
  
  ranger_rec_catch <- fit(
    bad_ranger_cls,
    engine = "ranger",
    .control = caught_ctrl,
    bad_lc_rec,
    data = lending_club
  )
  expect_true(inherits(ranger_rec_catch, "try-error"))    
})

test_that('randomForest execution', {
  skip_on_cran()
  
  expect_error(
    fit(
      lc_basic,
      engine = "randomForest",
      .control = ctrl,
      lc_form,
      data = lending_club
    ),
    regexp = NA
  )
  
  # fails during R CMD check but works outside of that
  # expect_error(
  #   fit(
  #     lc_basic,
  #     engine = "randomForest",
  #     .control = ctrl,
  #     x = lending_club[, num_pred],
  #     y = lending_club$Class
  #   ),
  #   regexp = NA
  # )
  
  expect_error(
    fit(
      lc_basic,
      engine = "randomForest",
      .control = ctrl,
      lc_rec,
      data = lending_club
    ),
    regexp = NA
  )
  
  expect_error(
    fit(
      bad_rf_cls,
      engine = "randomForest",
      .control = ctrl,
      lc_bad_form,
      data = lending_club
    )
  )
  
  randomForest_form_catch <- fit(
    bad_rf_cls,
    engine = "randomForest",
    .control = caught_ctrl,
    lc_bad_form,
    data = lending_club
  )
  expect_true(inherits(randomForest_form_catch, "try-error")) 
  
  randomForest_xy_catch <- fit(
    bad_rf_cls,
    engine = "randomForest",
    .control = caught_ctrl,
    x = lending_club[, num_pred],
    y = lending_club$total_bal_il
  )
  expect_true(inherits(randomForest_xy_catch, "try-error"))
  
  randomForest_rec_catch <- fit(
    bad_rf_cls,
    engine = "randomForest",
    .control = caught_ctrl,
    bad_lc_rec,
    data = lending_club
  )
  expect_true(inherits(randomForest_rec_catch, "try-error"))    
})


###################################################################

car_form <- as.formula(mpg ~ .)
num_pred <- names(mtcars)[3:6]

car_basic <- rand_forest(mode = "regression")

bad_ranger_reg <- rand_forest(mode = "regression", 
                              others = list(min.node.size = -10))
bad_rf_reg <- rand_forest(mode = "regression", 
                          others = list(sampsize = -10))

ctrl <- list(verbosity = 1, catch = FALSE)
caught_ctrl <- list(verbosity = 1, catch = TRUE)
quiet_ctrl <- list(verbosity = 0, catch = TRUE)

car_rec <- recipe(mpg ~ ., data = mtcars)



test_that('ranger execution', {
  skip_on_cran()
  
  expect_error(
    res <- fit(
      car_basic,
      engine = "ranger",
      .control = ctrl,
      car_form,
      data = mtcars
    ),
    regexp = NA
  )
  expect_error(
    res <- fit(
      car_basic,
      engine = "ranger",
      .control = ctrl,
      x = mtcars[, num_pred],
      y = mtcars$mpg
    ),
    regexp = NA
  )
  expect_error(
    res <- fit(
      car_basic,
      engine = "ranger",
      .control = ctrl,
      car_rec,
      data = mtcars
    ),
    regexp = NA
  )
  
  ranger_form_catch <- fit(
    bad_ranger_reg,
    engine = "ranger",
    .control = caught_ctrl,
    car_form,
    data = mtcars
  )
  expect_true(inherits(ranger_form_catch, "try-error")) 
  
  # fails
  # ranger_xy_catch <- fit(
  #   bad_ranger_reg,
  #   engine = "ranger",
  #   .control = caught_ctrl,
  #   x = mtcars[, num_pred],
  #   y = mtcars$total_bal_il
  # )
  # expect_true(inherits(ranger_xy_catch, "try-error"))
  
  ranger_rec_catch <- fit(
    bad_ranger_reg,
    engine = "ranger",
    .control = caught_ctrl,
    car_form,
    data = mtcars
  )
  expect_true(inherits(ranger_rec_catch, "try-error"))    
})

test_that('randomForest execution', {
  skip_on_cran()
  
  expect_error(
    fit(
      car_basic,
      engine = "randomForest",
      .control = ctrl,
      car_form,
      data = mtcars
    ),
    regexp = NA
  )
  
  expect_error(
    fit(
      car_basic,
      engine = "randomForest",
      .control = ctrl,
      x = mtcars[, num_pred],
      y = mtcars$mpg
    ),
    regexp = NA
  )
  
  expect_error(
    fit(
      car_basic,
      engine = "randomForest",
      .control = ctrl,
      car_rec,
      data = mtcars
    ),
    regexp = NA
  )
  
  randomForest_form_catch <- fit(
    bad_rf_reg,
    engine = "randomForest",
    .control = caught_ctrl,
    car_form,
    data = mtcars
  )
  expect_true(inherits(randomForest_form_catch, "try-error")) 
  
  randomForest_xy_catch <- fit(
    bad_rf_reg,
    engine = "randomForest",
    .control = caught_ctrl,
    x = mtcars[, num_pred],
    y = mtcars$total_bal_il
  )
  expect_true(inherits(randomForest_xy_catch, "try-error"))
  
  randomForest_rec_catch <- fit(
    bad_rf_reg,
    engine = "randomForest",
    .control = caught_ctrl,
    car_rec,
    data = mtcars
  )
  expect_true(inherits(randomForest_rec_catch, "try-error"))    
})

