#' Ordinal regression via vector GLMs
#'
#' [VGAM::vglm()] fits vector generalized linear models, which specialize to
#' several families of ordinal regression models.
#'
#' @includeRmd man/rmd/ordinal_reg_vglm.md details
#'
#' @name details_ordinal_reg_vglm
#' @keywords internal
NULL

# See inst/README-DOCS.md for a description of how these files are processed.

# ------------------------------------------------------------------------------
# Helpers for the `vglm` and `vgam` engines
#
# These functions match standardized dials parameter values to values native to
# `VGAM::vglm()` and `VGAM::vgam()`. The family helper is also used for the
# `ordinalNet` engine, which recognizes the same native families. They are used
# by `translate.ordinal_reg()` and `translate.gen_additive_mod()`.
values_ordinal_link_vglm <- c(
  dials::values_ordinal_link,
  # TODO: Expand to include link functions to other domains than [0,1].
  c("foldsqrt", "logc", "gord", "pord", "nbord")
)

values_threshold_structure_vglm <- c(
  dials::values_threshold_structure,
  "qnorm"
)

# TODO: Move to a more shared R script.
match_ordinal_family <- function(family) {
  if (!is.character(family)) {
    return(family)
  }
  if (family %in% c("cumulative", "acat", "cratio", "sratio")) {
    return(family)
  }
  family <- match.arg(family, dials::values_odds_link)
  switch(
    family,
    cumulative_link = "cumulative",
    adjacent_categories = "acat",
    continuation_ratio = "cratio",
    stopping_ratio = "sratio"
  )
}

match_ordinal_link_vglm <- function(link) {
  if (!is.character(link)) {
    return(link)
  }
  link_vgam <-
    # fmt: skip
    if (
      link %in% c(
        "logitlink", "probitlink", "logloglink", "clogloglink", "cauchitlink",
        "foldsqrtlink", "logclink", "gordlink", "pordlink", "nbordlink"
      )
    ) {
      link
    } else {
      link <- match.arg(link, values_ordinal_link_vglm)
      # REVIEW: Change `logistic` to `logit` in {dials}?
      if (link == "logistic") link <- "logit"
      paste0(link, "link")
    }
  if (link_vgam == "logloglink") {
    cli::cli_abort(
      c(
        "The `vglm` engine does not support the log-log ordinal link.",
        "i" = "See `?VGAM::Links` for provided link functions."
      )
    )
  }
  link_vgam
}

match_threshold_structure_vglm <- function(Thresh) {
  if (!is.character(Thresh)) {
    return(Thresh)
  }
  Thresh <- match.arg(Thresh, values_threshold_structure_vglm)
  switch(
    Thresh,
    flexible = "free",
    symmetric_median = "symm1",
    symmetric_zero = "symm0",
    equidistant = "equid",
    qnorm = "qnorm"
  )
}

check_ordinal_link_family_vglm <- function(family, link) {
  if (
    is.character(family) &&
      is.character(link) &&
      family == "acat" &&
      link %in% c("logitlink", "probitlink", "clogloglink")
  ) {
    cli::cli_abort(
      c(
        "The {.val adjacent_categories} family is not compatible with
         the {.val {link}} link function.",
        "i" = "Use {.val cauchitlink} or {.val identitylink} instead."
      )
    )
  }
  invisible(NULL)
}
