
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

###################################################################

surv_reg_flexsurv_fit <-
  list(
    libs = c("survival", "flexsurv"),
    interface = "formula",
    protect = c("formula", "data", "weights"),
    fit_name = c(pkg = "flexsurv", fun = "flexsurvreg"),
    alternates = list()
  )
