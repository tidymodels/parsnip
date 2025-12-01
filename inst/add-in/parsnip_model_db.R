# ------------------------------------------------------------------------------
# code to make the parsnip model database used by the RStudio addin

# ------------------------------------------------------------------------------

library(tidymodels)
library(usethis)

# also requires installation of:
packages <- c(
  "parsnip",
  parsnip:::extensions(),
  "modeltime"
  # "modeltime.gluonts" # required python packages to create spec
)

loaded <- map(packages, library, character.only = TRUE)

# ------------------------------------------------------------------------------

get_model <- function(x) {
  res <- get_from_env(x)
  if (!is.null(res)) {
    res <- dplyr::mutate(res, model = x)
  }
  res
}

get_packages <- function(x) {
  res <- get_from_env(paste0(x, "_pkgs"))
  if (is.null(res)) {
    return(res)
  }
  res <-
    res |>
    tidyr::unnest(pkg) |>
    dplyr::mutate(
      model = x
    )

  res
}

get_models <- function() {
  res <- ls(envir = get_model_env(), pattern = "_fit$")
  models <- gsub("_fit$", "", res)
  models <-
    purrr::map(models, get_model) |>
    purrr::list_rbind()

  # get source package
  pkgs <- gsub("_fit$", "_pkgs", res)
  pkgs <-
    unique(models$model) |>
    purrr::map(get_packages) |>
    purrr::list_rbind() |>
    dplyr::filter(pkg %in% packages)
  dplyr::left_join(models, pkgs, by = dplyr::join_by(engine, mode, model)) |>
    dplyr::rename(package = pkg) |>
    dplyr::mutate(
      package = dplyr::if_else(is.na(package), "parsnip", package),
      call_from_parsnip = package %in% parsnip:::extensions(),
      caller_package = dplyr::if_else(
        call_from_parsnip,
        "parsnip",
        package
      )
    )
}


get_engines <- function(x) {
  eng <- try(parsnip::show_engines(x), silent = TRUE)
  if (inherits(eng, "try-error")) {
    eng <- tibble::tibble(
      engine = NA_character_,
      mode = NA_character_,
      model = x
    )
  } else {
    eng$model <- x
  }
  eng
}
get_tunable_param <- function(mode, package, model, engine) {
  cl <- rlang::call2(.ns = package, .fn = model)
  obj <- rlang::eval_tidy(cl)
  obj <- parsnip::set_engine(obj, engine)
  obj <- parsnip::set_mode(obj, mode)
  res <-
    tune::tunable(obj) |>
    dplyr::select(parameter = name)

  # ------------------------------------------------------------------------------
  # Edit some model parameters

  if (model == "rand_forest") {
    res <- res[res$parameter != "trees", ]
  }
  if (model == "mars") {
    res <- res[res$parameter == "prod_degree", ]
  }
  if (engine %in% c("rule_fit", "xgboost")) {
    res <- res[res$parameter != "mtry", ]
  }
  if (model %in% c("bag_tree", "bag_mars")) {
    res <- res[0, ]
  }
  if (engine %in% c("rpart")) {
    res <- res[res$parameter != "tree-depth", ]
  }
  res
}

# ------------------------------------------------------------------------------

model_db <-
  get_models() |>
  dplyr::filter(mode %in% c("regression", "classification")) |>
  dplyr::filter(engine != "liquidSVM") |>
  dplyr::filter(model != "surv_reg") |>
  dplyr::filter(engine != "spark") |>
  dplyr::filter(!is.na(engine)) |>
  dplyr::mutate(label = paste0(model, " (", engine, ")")) |>
  dplyr::arrange(model, engine, mode)

num_modes <-
  model_db |>
  dplyr::group_by(package, model, engine) |>
  dplyr::count() |>
  dplyr::ungroup() |>
  dplyr::mutate(single_mode = n == 1) |>
  dplyr::select(package, model, engine, single_mode)

model_db <-
  dplyr::left_join(model_db, num_modes, by = c("package", "model", "engine")) |>
  dplyr::mutate(
    parameters = purrr::pmap(
      list(mode, caller_package, model, engine),
      get_tunable_param
    )
  ) |>
  dplyr::select(-call_from_parsnip, -caller_package)

usethis::use_data(model_db, overwrite = TRUE)
