library(modeldata)

data("wa_churn")
data("lending_club")
data("hpc_data")

# ------------------------------------------------------------------------------

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

# ------------------------------------------------------------------------------
# for quantile regression tests

data("Sacramento")

Sacramento_small <-
  modeldata::Sacramento %>%
  dplyr::mutate(price = log10(price)) %>%
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

