
surv_reg_arg_key <- data.frame(
  flexsurv =  c("dist"),
  survreg  =  c("dist"),
  stringsAsFactors = FALSE,
  row.names =  c("dist")
)

surv_reg_modes <- "regression"

surv_reg_engines <- data.frame(
  flexsurv = TRUE,
  survreg  = TRUE,
  stringsAsFactors    = TRUE,
  row.names =  c("regression")
)

# ------------------------------------------------------------------------------

surv_reg_flexsurv_data <-
  list(
    libs = c("survival", "flexsurv"),
    fit = list(
      interface = "formula",
      protect = c("formula", "data", "weights"),
      func = c(pkg = "flexsurv", fun = "flexsurvreg"),
      defaults = list()
    ),
    numeric = list(
      pre = NULL,
      post = flexsurv_mean,
      func = c(fun = "summary"),
      args =
        list(
          object = expr(object$fit),
          newdata = expr(new_data),
          type = "mean"
        )
    ),
    quantile = list(
      pre = NULL,
      post = flexsurv_quant,
      func = c(fun = "summary"),
      args =
        list(
          object = expr(object$fit),
          newdata = expr(new_data),
          type = "quantile",
          quantiles = expr(quantile)
        )
    )
  )

# ------------------------------------------------------------------------------

surv_reg_survreg_data <-
  list(
    libs = c("survival"),
    fit = list(
      interface = "formula",
      protect = c("formula", "data", "weights"),
      func = c(pkg = "survival", fun = "survreg"),
      defaults = list(model = TRUE)
    ),
    numeric = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args =
        list(
          object = expr(object$fit),
          newdata = expr(new_data),
          type = "response"
        )
    ),
    quantile = list(
      pre = NULL,
      post = survreg_quant,
      func = c(fun = "predict"),
      args =
        list(
          object = expr(object$fit),
          newdata = expr(new_data),
          type = "quantile",
          p = expr(quantile)
        )
    )
  )

# ------------------------------------------------------------------------------

# surv_reg_stan_data <-
#   list(
#     libs = c("brms"),
#     fit = list(
#       interface = "formula",
#       protect = c("formula", "data", "weights"),
#       func = c(pkg = "brms", fun = "brm"),
#       defaults = list(
#         family = expr(brms::weibull()),
#         seed = expr(sample.int(10^5, 1))
#       )
#     ),
#     numeric = list(
#       pre = NULL,
#       post = function(results, object) {
#         tibble::as_tibble(results) %>%
#           dplyr::select(Estimate) %>%
#           setNames(".pred")
#       },
#       func = c(fun = "predict"),
#       args =
#         list(
#           object = expr(object$fit),
#           newdata = expr(new_data),
#           type = "response"
#         )
#     )
#   )

