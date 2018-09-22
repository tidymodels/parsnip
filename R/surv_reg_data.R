
surv_reg_arg_key <- data.frame(
  flexsurv    =  c("dist", NA),
  stringsAsFactors = FALSE,
  row.names =  c("dist", "mixture")
)

surv_reg_modes <- "regression"

surv_reg_engines <- data.frame(
  flexsurv = TRUE,
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
    )
  )
