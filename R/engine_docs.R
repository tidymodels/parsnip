#' Knit engine-specific documentation
#' @param pattern A regular expression to specify which files to knit. The
#' default knits all engine documentation files.
#' @return A tibble with column `file` for the file name and `result` (a
#' character vector that echos the output file name or, when there is
#' a failure, the error message).
#' @details
#' This function will check whether the known parsnip extension packages,
#' engine specific packages, and a few other ancillary packages are installed.
#' Users will be prompted to install anything required to create the engine
#' documentation.
#'
#' @keywords internal
#' @export
knit_engine_docs <- function(pattern = NULL) {
  install_engine_packages()
  old_cli_opt <- options()$cli.unicode
  on.exit(options(cli.unicode = old_cli_opt))
  options(cli.unicode = FALSE)

  rmd_files <- list.files("man/rmd", pattern = "\\.Rmd", full.names = TRUE)

  if (!is.null(pattern)) {
    target_exists <- grepl(pattern, rmd_files)
    files <- rmd_files[target_exists]
  } else {
    files <- rmd_files[!grepl("(template-)|(setup\\.)|(aaa\\.)", rmd_files)]
  }
  outputs <- gsub("Rmd$", "md", files)

  res <- purrr::map2(files, outputs, ~ try(knitr::knit(.x, .y), silent = TRUE))
  is_error <- purrr::map_lgl(res, ~ inherits(.x, "try-error"))

  if (any(is_error)) {
    # In some cases where there are issues, the md file is empty.
    errors <- res[which(is_error)]
    error_nms <- basename(files)[which(is_error)]
    errors <-
      purrr::map_chr(errors, ~ cli::ansi_strip(as.character(.x))) |>
      purrr::map2_chr(error_nms, ~ paste0(.y, ": ", .x)) |>
      purrr::map_chr(~ gsub("Error in .f(.x[[i]], ...) :", "", .x, fixed = TRUE))
    cat("There were failures duing knitting:\n\n")
    cat(errors)
    cat("\n\n")
  }

  res <- purrr::map_chr(res, as.character)

  issues <- list_md_problems()
  if (nrow(issues) > 0) {
    cat("There are some issues with the help files:\n")
    print(issues)
  }

  invisible(tibble::tibble(file = basename(files), result = res))
}

# ------------------------------------------------------------------------------

extensions <- function() {
  c("baguette", "censored", "discrim", "multilevelmod", "plsmod",
    "poissonreg", "rules", "bonsai", "agua")
}

# ------------------------------------------------------------------------------

#' Save information about models
#' @description
#' This function writes a tab delimited file to the package to capture
#' information about the known models. This information includes packages in
#' the tidymodels GitHub repository as well as packages that are known to work
#' well with tidymodels packages (e.g. not only \pkg{parsnip} but also
#' \pkg{tune}, etc.). There may be more model definitions in other extension
#' packages that are not included here.
#'
#' These data are used to document engines for each model function man page.
#' @keywords internal
#' @param path A character string for the location of the tab delimited file.
#' @details
#' See our
#' [model implementation guidelines](https://tidymodels.github.io/model-implementation-principles/)
#' on best practices for modeling and modeling packages.
#'
#' It is highly recommended that the known parsnip extension packages are loaded.
#' The unexported \pkg{parsnip} function `extensions()` will list these.
#' @export
update_model_info_file <- function(path = "inst/models.tsv") {
  mods <- get_from_env("models")
  info <-
    purrr::map(mods, \(x) get_from_env(x) |> dplyr::mutate(model = x)) |>
    purrr::list_rbind() |>
    dplyr::arrange(model, mode, engine) |>
    dplyr::select(model, mode, engine)
  exts <-
    purrr::map(
      mods,
      \(x) get_from_env(paste0(x, "_pkgs")) |> dplyr::mutate(model = x)
    ) |>
    purrr::list_rbind() |>
    tidyr::unnest(cols = "pkg") |>
    dplyr::inner_join(tibble::tibble(pkg = extensions()), by = "pkg")

  info <- dplyr::left_join(info, exts, by = c("model", "engine", "mode"))

  csv <- utils::write.table(info, file = path, row.names = FALSE, sep = "\t")
  invisible(info)
}

# ------------------------------------------------------------------------------


#' Tools for documenting engines
#'
#' @description
#' parsnip has a fairly complex documentation system where the engines for
#'  each model have detailed documentation about the syntax, tuning parameters,
#'  preprocessing needs, and so on.
#'
#' The functions below are called from `.R` files to programmatically
#'  generate content in the help files for a model.
#'
#'   * [find_engine_files()] identifies engines for a model and creates a
#'  bulleted list of links to those specific help files.
#'
#'   * [make_seealso_list()] creates a set of links for the "See Also" list at
#'  the bottom of the help pages.
#'
#'   * [find_engine_files()] is a function, used by the above, to find the
#'  engines for each model function.
#'
#' @param mod A character string for the model file (e.g. "linear_reg")
#' @param pkg A character string for the package where the function is invoked.
#' @return
#' `make_engine_list()` returns a character string that creates a
#' bulleted list of links to more specific help files.
#'
#' `make_seealso_list()` returns a formatted character string of links.
#'
#' `find_engine_files()` returns a tibble.
#' @details
#' parsnip includes a document (`README-DOCS.md`) with step-by-step instructions
#' and details. See the code below to determine where it is installed (or see
#' the References section).
#'
#' Most parsnip users will not need to use these functions or documentation.
#' @references
#' \url{https://github.com/tidymodels/parsnip/blob/main/inst/README-DOCS.md}
#' @name doc-tools
#' @keywords internal
#' @export
#' @examplesIf !parsnip:::is_cran_check()
#' # See this file for step-by-step instructions.
#' system.file("README-DOCS.md", package = "parsnip")
#'
#' # Code examples:
#' make_engine_list("linear_reg")
#'
#' cat(make_engine_list("linear_reg"))
find_engine_files <- function(mod) {
  # Get available topics
  topic_names <- find_details_topics(mod)
  if (length(topic_names) == 0) {
    return(character(0))
  }

  # Subset for our model function
  prefix <- paste0("parsnip:details_", mod, "_")
  eng <- gsub(prefix, "", topic_names)
  eng <- tibble::tibble(engine = eng, topic = topic_names)

  # Determine and label default engine
  default <- get_default_engine(mod)
  eng$default <- ifelse(eng$engine == default, cli::symbol$sup_1, "")

  # reorder based on default and name
  non_defaults <- dplyr::filter(eng, default == "")
  non_defaults <-
    non_defaults |>
    dplyr::arrange(tolower(engine)) |>
    dplyr::mutate(.order = dplyr::row_number() + 1)
  eng <-
    dplyr::filter(eng, default != "") |>
    dplyr::mutate(.order = 1) |>
    dplyr::bind_rows(non_defaults)

  eng
}

#' @export
#' @rdname doc-tools
make_engine_list <- function(mod) {
  eng <- find_engine_files(mod)

  if (length(eng) == 0) {
    return(
      paste(
        "There are different ways to fit this model. The method of estimation is ",
        "chosen by setting the model \\emph{engine}. No engines were found for ",
        "this model.\n\n"
      )
    )
  } else {
    main <- paste(
      "There are different ways to fit this model, and the method of estimation is ",
      "chosen by setting the model \\emph{engine}. The engine-specific pages ",
      "for this model are listed  below.\n\n"
    )
  }

  exts <-
    model_info_table |>
    dplyr::filter(model == mod) |>
    dplyr::group_by(engine, mode) |>
    dplyr::summarize(extensions = sum(!is.na(pkg)), .groups = "drop") |>
    dplyr::mutate(
      has_ext = ifelse(extensions > 0, cli::symbol$sup_2, "")
    )
  eng <- dplyr::left_join(eng, exts, by = "engine")


  eng_table <-
    eng |>
    dplyr::arrange(.order) |>
    dplyr::select(-mode) |>
    dplyr::distinct(engine, .keep_all = TRUE) |>
    dplyr::mutate(
      item = glue::glue("  \\item \\code{\\link[|topic|]{|engine|}|default||has_ext|}",
                        .open = "|", .close = "|")
    )

  notes <- paste0("\n", cli::symbol$sup_1, " The default engine.")
  if (any(exts$has_ext != "")) {
    if (dplyr::n_distinct(exts$mode) > 1) {
      ext_modes <- exts |>
        dplyr::filter(has_ext != "") |>
        dplyr::pull(mode) |>
        unique() |>
        sort() |>
        combine_words()
      notes <- paste0(
        notes, " ",
        cli::symbol$sup_2, " Requires a parsnip extension package for ",
        ext_modes, ".")
    } else {
      notes <- paste0(notes, " ", cli::symbol$sup_2, " Requires a parsnip extension package.")
    }
  }


  items <- glue::glue_collapse(eng_table$item, sep = "\n")
  res <- glue::glue("|main|\n\\itemize{\n|items|\n}\n\n |notes|",
                    .open = "|", .close = "|")
  res
}

get_default_engine <- function(mod, pkg = "parsnip") {
  cl <- rlang::call2(mod, .ns = pkg)
  suppressMessages(
    res <- rlang::eval_tidy(cl)$engine
  )
  res
}

#' @export
#' @rdname  doc-tools
make_seealso_list <- function(mod, pkg= "parsnip") {
  requireNamespace(pkg, quietly = TRUE)
  eng <- find_engine_files(mod)

  main <- c("\\code{\\link[=fit.model_spec]{fit()}}",
            "\\code{\\link[=set_engine]{set_engine()}}",
            "\\code{\\link[=update]{update()}}")

  if (length(eng) == 0) {
    return(paste0(main, collapse = ", "))
  }

  res <-
    glue::glue("\\code{\\link[|eng$topic|]{|eng$engine| engine details}}",
               .open = "|", .close = "|")

  if (pkg != "parsnip") {
    main <- NULL
  }
  paste0(c(main, res), collapse = ", ")
}

read_rd_rds <- function(pkg) {
  quiet_sys_file <- purrr::quietly(system.file)
  file_loc <- quiet_sys_file("Meta/Rd.rds", package = pkg)

  if (length(file_loc$result) == 0) {
    res <- character(0)
  } else {
    res <- file_loc$result
  }
  res
}


find_details_topics <- function(mod, pkg = "parsnip") {
  meta_loc <- read_rd_rds(pkg = pkg)
  meta_loc <- meta_loc[meta_loc != ""]
  if (length(meta_loc) > 0) {
    topic_names <- readRDS(meta_loc)$Name
    res <- grep(paste0("details_", mod), topic_names, value = TRUE)
    if (length(res) > 0) {
      res <- paste0(pkg, ":", res)
    }
  } else {
    res <- character(0)
  }
  unique(res)
}

sort_c <- function(x) {
  withr::with_collate("C", sort(x))
}

# ------------------------------------------------------------------------------

#' Locate and show errors/warnings in engine-specific documentation
#' @return A tibble with column `file` for the file name, `line` indicating
#'   the line where the error/warning occurred, and `problem` showing the
#'   error/warning message.
#' @keywords internal
#' @export
list_md_problems <- function() {
  md_files <- list.files("man/rmd", pattern = "\\.md", full.names = TRUE)

  get_errors <- function(file) {
    lines <- readLines(file)
    line <- grep("## (Error|Warning)", lines)
    problem <- lines[line]
    tibble(basename(file), line, problem)
  }

  purrr::map(md_files, get_errors) |> purrr::list_rbind()
}
