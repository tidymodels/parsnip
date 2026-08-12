extract_engine_doc_match <- function(pattern, text) {
  match <- regexec(pattern, text, perl = TRUE)
  values <- regmatches(text, match)[[1]]
  if (length(values) == 0) {
    return(NULL)
  }
  values[-1]
}

prediction_doc_chunk <- function(lines) {
  labels <- grep("^#\\| label: predict-types\\s*$", lines)
  if (length(labels) != 1) {
    return(NULL)
  }

  closing_fence <- grep("^```\\s*$", lines[(labels + 1):length(lines)])
  if (length(closing_fence) == 0) {
    return(NULL)
  }

  end <- labels + closing_fence[[1]] - 1
  paste(lines[labels:end], collapse = "\n")
}

prediction_doc_output <- function(lines) {
  heading <- which(lines == "## Prediction types")
  if (length(heading) != 1) {
    return(NULL)
  }

  next_heading <- grep("^## [^#]", lines[(heading + 1):length(lines)])
  if (length(next_heading) == 0) {
    end <- length(lines)
  } else {
    end <- heading + next_heading[[1]] - 1
  }

  paste(lines[heading:end], collapse = "\n")
}

test_that("engine documentation uses matching prediction registrations", {
  rmd_dir <- test_path("..", "..", "man", "rmd")
  model_info <- model_info_table
  rmd_files <- list.files(rmd_dir, pattern = "\\.Rmd$", full.names = TRUE)
  problems <- character()

  for (rmd_file in rmd_files) {
    lines <- readLines(rmd_file, warn = FALSE)
    text <- paste(lines, collapse = "\n")
    declared <- extract_engine_doc_match(
      'descr_models\\("([^"]+)",\\s*"([^"]+)"\\)',
      text
    )

    if (is.null(declared) && basename(rmd_file) != "null-model.Rmd") {
      next
    }

    if (basename(rmd_file) == "null-model.Rmd") {
      declared <- c("null_model", "parsnip")
    }

    chunk <- prediction_doc_chunk(lines)
    if (is.null(chunk)) {
      problems <- c(
        problems,
        paste0(basename(rmd_file), " must have one prediction-types chunk.")
      )
      next
    }

    registry <- extract_engine_doc_match(
      'get_from_env\\("([^"]+)_predict"\\)',
      chunk
    )
    if (is.null(registry) || registry[[1]] != declared[[1]]) {
      problems <- c(
        problems,
        paste0(
          basename(rmd_file),
          " queries ",
          if (is.null(registry)) "no prediction registry" else registry[[1]],
          "; expected ",
          declared[[1]],
          "."
        )
      )
    }

    engine <- extract_engine_doc_match('engine\\s*==\\s*"([^"]+)"', chunk)
    if (is.null(engine)) {
      engine <- extract_engine_doc_match(
        'str_starts\\(engine,\\s*"([^"]+)"',
        chunk
      )
    }

    known_engines <- unique(model_info$engine[
      model_info$model == declared[[1]]
    ])
    if (length(known_engines) > 1 && is.null(engine)) {
      problems <- c(
        problems,
        paste0(
          basename(rmd_file),
          " must filter prediction types to engine ",
          declared[[2]],
          "."
        )
      )
    } else if (!is.null(engine) && engine[[1]] != declared[[2]]) {
      problems <- c(
        problems,
        paste0(
          basename(rmd_file),
          " filters engine ",
          engine[[1]],
          "; expected ",
          declared[[2]],
          "."
        )
      )
    }

    md_file <- sub("\\.Rmd$", ".md", rmd_file)
    if (!file.exists(md_file)) {
      problems <- c(
        problems,
        paste0(basename(md_file), " has not been generated.")
      )
      next
    }

    output <- prediction_doc_output(readLines(md_file, warn = FALSE))
    if (is.null(output)) {
      problems <- c(
        problems,
        paste0(basename(md_file), " has no generated prediction section.")
      )
    } else if (grepl("A tibble: 0 (x|×)", output)) {
      problems <- c(
        problems,
        paste0(basename(md_file), " contains an empty prediction table.")
      )
    }
  }

  expect_identical(problems, character())
})
