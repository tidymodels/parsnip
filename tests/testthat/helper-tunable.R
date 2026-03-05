# Helper function for displaying tunable call_info in snapshot tests
# This expands and displays all call_info elements in a format suitable for
# snapshot testing, capturing pkg, fun, range, and values.

display_tunable_call_info <- function(model_spec) {
  res <- tunable(model_spec)

  if (nrow(res) == 0) {
    cat("No tunable parameters.\n")
    return(invisible(res))
  }

  for (i in seq_len(nrow(res))) {
    param_name <- res$name[i]
    call_info <- res$call_info[[i]]
    component_id <- res$component_id[i]

    # Build the info string
    info_parts <- character(0)

    if (!is.null(call_info$pkg)) {
      info_parts <- c(info_parts, paste0("pkg: ", call_info$pkg))
    }
    if (!is.null(call_info$fun)) {
      info_parts <- c(info_parts, paste0("fun: ", call_info$fun))
    }
    if (!is.null(call_info$range)) {
      range_str <- paste0(
        "c(",
        paste(format(call_info$range, nsmall = 2), collapse = ", "),
        ")"
      )
      info_parts <- c(info_parts, paste0("range: ", range_str))
    }
    if (!is.null(call_info$values)) {
      values_str <- paste(call_info$values, collapse = ", ")
      info_parts <- c(info_parts, paste0("values: ", values_str))
    }

    info_str <- paste(info_parts, collapse = ", ")

    # Pad the parameter name for alignment
    padded_name <- format(param_name, width = 25)

    cat(paste0(padded_name, " | ", info_str, " | ", component_id, "\n"))
  }

  invisible(res)
}
