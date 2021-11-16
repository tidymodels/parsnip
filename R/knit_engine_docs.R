#' Knit engine-specific documentation
#' @param pattern A regular expression to specify which files to knit. The
#' default knits all engine documentation files.
#' @return A tibble with column `file` for the file name and `result` (a
#' character vector that echos the output file name or, when there is
#' a failure, the error message).
#' @keywords internal
#' @export
knit_engine_docs <- function(pattern = NULL) {
  rmd_files <- list.files("man/rmd", pattern = "\\.Rmd", full.names = TRUE)

  if (!is.null(pattern)) {
    target_exists <- grepl(pattern, rmd_files)
    files <- rmd_files[target_exists]
  } else {
    files <- rmd_files[!grepl("(template-)|(setup\\.)", rmd_files)]
  }
  outputs <- gsub("Rmd$", "md", files)

  res <- purrr::map2(files, outputs, ~ try(knitr::knit(.x, .y), silent = TRUE))
  res <- purrr::map_chr(res, as.character)
  tibble::tibble(file = basename(files), result = res)
}

# TODO
# - simplify code to find model files
# - add is_installed() to set code with all extra dependencies
# - list models by mode


extensions <- function(x) {
  c("baguette", "censored", "discrim", "multilevelmod", "plsmod",
    "poissonreg", "rules")
}
