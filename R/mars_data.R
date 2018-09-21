
mars_arg_key <- data.frame(
  earth =      c(   "nprune",      "degree",      "pmethod"),
  stringsAsFactors = FALSE,
  row.names =  c("num_terms", "prod_degree", "prune_method")
)

mars_modes <- c("classification", "regression", "unknown")

mars_engines <- data.frame(
  earth =    rep(TRUE, 3),
  row.names =  c("classification", "regression", "unknown")
)

# earth helpers ----------------------------------------------------------------

#' @importFrom purrr map_df
#' @importFrom dplyr arrange
#' @export
multi_predict._earth <-
  function(object, new_data, type = NULL, num_terms = NULL, ...) {
    if (is.null(num_terms))
      num_terms <- object$fit$selected.terms[-1]

    num_terms <- sort(num_terms)

    msg <-
      paste("Please use `keepxy = TRUE` as an option to enable submodel",
            "predictions with `earth`.")
    if (any(names(object$spec$others) == "keepxy")) {
      if(!object$spec$others$keepxy)
        stop (msg, call. = FALSE)
    } else
      stop (msg, call. = FALSE)

    if (!exists("earth"))
      suppressPackageStartupMessages(attachNamespace("earth"))

    if (is.null(type)) {
      if (object$spec$mode == "classification")
        type <- "class"
      else
        type <- "numeric"
    }

    res <-
      map_df(num_terms, earth_by_terms, object = object,
             new_data = new_data, type = type, ...)
    res <- arrange(res, .row, num_terms)
    res <- split(res[, -1], res$.row)
    names(res) <- NULL
    tibble(.pred = res)
  }

earth_by_terms <- function(num_terms, object, new_data, type, ...) {
  object$fit <- update(object$fit, nprune = num_terms)
  pred <- predict(object, new_data = new_data, type = type)
  nms <- names(pred)
  pred[["num_terms"]] <- num_terms
  pred[[".row"]] <- 1:nrow(new_data)
  pred[, c(".row", "num_terms", nms)]
}

# ------------------------------------------------------------------------------

mars_earth_data <-
  list(
    libs = "earth",
    fit = list(
      interface = "data.frame",
      protect = c("x", "y", "weights"),
      func = c(pkg = "earth", fun = "earth"),
      defaults =
        list()
    ),
    pred = list(
      pre = NULL,
      post = maybe_multivariate,
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(new_data),
          type = "response"
        )
    ),
    classes = list(
      pre = NULL,
      post = function(x, object) {
        x <- ifelse(x[,1] >= 0.5, object$lvl[2], object$lvl[1])
        x
      },
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(new_data),
          type = "response"
        )
    ),
    prob = list(
      pre = NULL,
      post = function(x, object) {
        x <- x[,1]
        x <- tibble(v1 = 1 - x, v2 = x)
        colnames(x) <- object$lvl
        x
      },
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(new_data),
          type = "response"
        )
    ),
    raw = list(
      pre = NULL,
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(new_data)
        )
    )
  )

