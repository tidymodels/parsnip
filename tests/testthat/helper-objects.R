ctrl          <- control_parsnip(verbosity = 1, catch = FALSE)
caught_ctrl   <- control_parsnip(verbosity = 1, catch = TRUE)
quiet_ctrl    <- control_parsnip(verbosity = 0, catch = TRUE)

run_glmnet <- utils::compareVersion('3.6.0', as.character(getRversion())) > 0

# ------------------------------------------------------------------------------
# for skips

is_tf_ok <- function() {
  tf_ver <- try(tensorflow::tf_version(), silent = TRUE)
  if (inherits(tf_ver, "try-error")) {
    res <- FALSE
  } else {
    res <- !is.null(tf_ver)
  }
  res
}

if (rlang::is_installed("modeldata")) {
  # ------------------------------------------------------------------------------

  library(modeldata)

  data("wa_churn")
  data("lending_club")
  data("hpc_data")
  data(two_class_dat, package = "modeldata")

  # ------------------------------------------------------------------------------

  hpc <- hpc_data[1:150, c(2:5, 8)]
  num_hpc_pred <- names(hpc)[1:4]
  class_tab <- table(hpc$class, dnn = NULL)
  hpc_bad <-
    hpc |>
    dplyr::mutate(big_num = Inf)

  set.seed(352)
  mlp_dat <- hpc[order(runif(150)),]

  tr_mlp_dat <- mlp_dat[1:140, ]
  te_mlp_dat <- mlp_dat[141:150, ]

  mars_hpc_pred_list <- colnames(hpc)[1:3]
  mlp_hpc_pred_list <- names(hpc)[1:4]
  nnet_hpc_pred_list <- names(hpc)[1:4]

  hpc_nnet_dat <- hpc_data[1:150, c(2:5, 8)]

  # ------------------------------------------------------------------------------

  lm_fit <-
    linear_reg(mode = "regression") |>
    set_engine("lm") |>
    fit(compounds ~ ., data = hpc)

  lending_club <-
    lending_club |>
    dplyr::slice(1:200) |>
    dplyr::mutate(big_num = Inf)

  lending_lvl <- levels(lending_club$Class)

  # ------------------------------------------------------------------------------
  # for quantile regression tests

  data("Sacramento")

  Sacramento_small <-
    modeldata::Sacramento |>
    dplyr::mutate(price = log10(price)) |>
    dplyr::select(price, beds, baths, sqft, latitude, longitude)

  sac_train <- Sacramento_small[-(1:5), ]
  sac_test  <- Sacramento_small[  1:5 , ]

  # ------------------------------------------------------------------------------
  # For sparse tibble testing

  sparse_hotel_rates <- function(tibble = FALSE) {
    # 99.2 sparsity
    hotel_rates <- modeldata::hotel_rates

    prefix_colnames <- function(x, prefix) {
      colnames(x) <- paste(colnames(x), prefix, sep = "_")
      x
    }

    dummies_country <- hardhat::fct_encode_one_hot(hotel_rates$country)
    dummies_company <- hardhat::fct_encode_one_hot(hotel_rates$company)
    dummies_agent <- hardhat::fct_encode_one_hot(hotel_rates$agent)

    res <- dplyr::bind_cols(
      hotel_rates["avg_price_per_room"],
      prefix_colnames(dummies_country, "country"),
      prefix_colnames(dummies_company, "company"),
      prefix_colnames(dummies_agent, "agent")
    )

    res <- as.matrix(res)
    res <- Matrix::Matrix(res, sparse = TRUE)

    if (tibble) {
      res <- sparsevctrs::coerce_to_sparse_tibble(res)

      # materialize outcome
      withr::local_options("sparsevctrs.verbose_materialize" = NULL)
      res$avg_price_per_room <- res$avg_price_per_room[]
    }

    res
  }
}

if (rlang::is_installed("survival")) {
  data(cancer, package = "survival")
  basic_form <- survival::Surv(time, status) ~ age
  complete_form <- survival::Surv(time) ~ age

  if (R.Version()$major < "4") {
    data(lung, package = 'survival')
  } else {
    data(cancer, package = 'survival')
  }

  basic_form <- survival::Surv(time, status) ~ group
  complete_form <- survival::Surv(time) ~ group
}
