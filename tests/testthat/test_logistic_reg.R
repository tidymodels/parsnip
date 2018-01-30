library(testthat)
library(parsnip)


test_that('primary arguments', {
  basic <- logistic_reg()
  basic_glm <- finalize(basic, engine = "glm")  
  basic_glmnet <- finalize(basic, engine = "glmnet")    
  basic_stan <- finalize(basic, engine = "stan_glm")    
  expect_equal(basic_glm$method$fit,
               quote(
                 glm(
                   formula = formula,
                   family = binomial(),
                   data = data,
                   weights = NULL
                 )
               )
  )  
  expect_equal(basic_glmnet$method$fit,
               quote(
                 glmnet(
                   x = as.matrix(x), 
                   y = y, 
                   family = "binomial"
                   )
               )
  )    
  expect_equal(basic_stan$method$fit,
               quote(
                 stan_glm(
                   formula = formula,
                   family = binomial(),
                   data = data,
                   weights = NULL
                 )
               )
  )     
  
  mixture <- logistic_reg(mixture = 0.128)
  mixture_glmnet <- finalize(mixture, engine = "glmnet")
  expect_equal(mixture_glmnet$method$fit,
               quote(
                 glmnet(
                   x = as.matrix(x),
                   y = y,
                   family = "binomial",
                   alpha = 0.128
                 )
               )
  )  
  
  regularization <- logistic_reg(regularization = 1)
  regularization_glmnet <- finalize(regularization, engine = "glmnet")
  expect_equal(regularization_glmnet$method$fit,
               quote(
                 glmnet(
                   x = as.matrix(x),
                   y = y,
                   family = "binomial",
                   lambda = 1
                 )
               )
  )    

  mixture_v <- logistic_reg(mixture = varying())
  mixture_v_glmnet <- finalize(mixture_v, engine = "glmnet")
  expect_equal(mixture_v_glmnet$method$fit,
               quote(
                 glmnet(
                   x = as.matrix(x),
                   y = y,
                   family = "binomial",
                   alpha = varying()
                 )
               )
  )  
})

test_that('engine arguments', {
  glm_fam <- logistic_reg(engine_args = list(family = binomial(link = "probit")))
  expect_equal(finalize(glm_fam, engine = "glm")$method$fit,
               quote(
                 glm(
                   formula = formula,
                   family = binomial(link = "probit"),
                   data = data,
                   weights = NULL
                 )
               )
  )
  
  glmnet_nlam <- logistic_reg(engine_args = list(nlambda = 10))
  expect_equal(finalize(glmnet_nlam, engine = "glmnet")$method$fit,
               quote(
                 glmnet(
                   x = as.matrix(x),
                   y = y,
                   family = "binomial",
                   nlambda = 10
                 )
               )
  ) 
  
  # these should get pass into the ... slot
  stan_samp <- logistic_reg(engine_args = list(chains = 1, iter = 5))
  expect_equal(finalize(stan_samp, engine = "stan_glm")$method$fit,
               quote(
                 stan_glm(
                   formula = formula,
                   family = binomial(),
                   data = data,
                   weights = NULL,
                   chains = 1, 
                   iter = 5
                 )
               )
  ) 

})


test_that('updating', {
  expr1     <- logistic_reg(             engine_args = list(family = binomial(link = "probit")))
  expr1_exp <- logistic_reg(mixture = 0, engine_args = list(family = binomial(link = "probit")))
  
  expr2     <- logistic_reg(mixture = varying())
  expr2_exp <- logistic_reg(mixture = varying(), engine_args = list(nlambda = 10))
  
  expr3     <- logistic_reg(mixture = 0, regularization = varying())
  expr3_exp <- logistic_reg(mixture = 1)
  
  expr4     <- logistic_reg(mixture = 0, engine_args = list(nlambda = 10))
  expr4_exp <- logistic_reg(mixture = 0, engine_args = list(nlambda = 10, pmax = 2))
  
  expr5     <- logistic_reg(mixture = 1, engine_args = list(nlambda = 10))
  expr5_exp <- logistic_reg(mixture = 1, engine_args = list(nlambda = 10, pmax = 2))
  
  expect_equal(update(expr1, mixture = 0), expr1_exp)
  expect_equal(update(expr2, engine_args = list(nlambda = 10)), expr2_exp)  
  expect_equal(update(expr3, mixture = 1, fresh = TRUE), expr3_exp)  
  expect_equal(update(expr4, engine_args = list(pmax = 2)), expr4_exp)
  expect_equal(update(expr5, engine_args = list(nlambda = 10, pmax = 2)), expr5_exp)
  
})

test_that('bad input', {
  expect_error(logistic_reg(ase.weights = var))
  expect_error(logistic_reg(mode = "regression"))
  expect_error(finalize(logistic_reg(), engine = "wat?"))
  expect_warning(finalize(logistic_reg(), engine = NULL))  
  expect_error(finalize(logistic_reg(engine_args = list(ytest = 2)), engine = "glmnet"))
  expect_error(finalize(logistic_reg(formula = y ~ x)))
  expect_warning(finalize(logistic_reg(engine_args = list(x = x, y = y)), engine = "glmnet"))
  expect_warning(finalize(logistic_reg(engine_args = list(formula = y ~ x)), engine = "glm"))
})


