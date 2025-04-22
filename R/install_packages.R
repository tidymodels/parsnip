# Installs packages needed to run `knit_engine_docs()`.
install_engine_packages <- function(extension = TRUE, extras = TRUE,
                                    ignore_pkgs = c("stats", "liquidSVM",
                                                    "parsnip")) {
  bio_pkgs <- c()

  if (extension) {
    extensions_packages <- extensions()
    rlang::check_installed(extensions_packages)
    bio_pkgs <- c(bio_pkgs, "mixOmics")
  }

  engine_packages <- purrr::map(
    ls(envir = get_model_env(), pattern = "_pkgs$"),
    get_from_env
  ) |>
    purrr::list_rbind() |>
    dplyr::pull(pkg) |>
    unlist() |>
    unique() |>
    setdiff(ignore_pkgs) |>
    setdiff(bio_pkgs)

  if (extension) {
    engine_packages <- setdiff(engine_packages, extensions_packages)
  }

  if (extras) {
    rmd_pkgs <- c("ape", "broom.mixed", "Cubist", "glmnet", "quantreg",
                  "rmarkdown", "tidymodels", "xrf")
    engine_packages <- unique(c(engine_packages, rmd_pkgs))
  }

  rlang::check_installed(engine_packages)
  rlang::check_installed(bio_pkgs)
}
