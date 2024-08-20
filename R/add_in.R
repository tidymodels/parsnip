#' Start an RStudio Addin that can write model specifications
#'
#' `parsnip_addin()` starts a process in the RStudio IDE Viewer window
#' that allows users to write code for parsnip model specifications from
#' various R packages. The new code is written to the current document at the
#' location of the cursor.
#'
#' @export
parsnip_addin <- function() {
  sys.source(
    system.file("add-in", "gadget.R", package = "parsnip", mustWork = TRUE),
    envir = rlang::new_environment(parent = rlang::global_env()),
    keep.source = FALSE
  )
}
