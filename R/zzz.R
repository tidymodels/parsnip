## nocov start

data_obj <- ls(pattern = "_data$")
data_obj <- data_obj[data_obj != "prepare_data"]

#' @importFrom purrr map_dfr
#' @importFrom tibble as_tibble
data_names <-
  map_dfr(
    data_obj,
    function(x)  {
      module <- names(get(x))
      if (length(module) > 1) {
        module <- table(module)
        module <- as_tibble(module)
        module$object <- x
        module
      } else
        module <- NULL
      module
    }
  )

if(any(data_names$n > 1)) {
  print(data_names[data_names$n > 1,])
  stop("Some models have duplicate module names.")
}
rm(data_names)

# ------------------------------------------------------------------------------

engine_objects <- ls(pattern = "_engines$")
engine_objects <- engine_objects[engine_objects != "possible_engines"]

#' @importFrom utils stack
get_engine_info <- function(x) {
  y <- x
  y <- get(y)
  z <- stack(y)
  z$mode <- rownames(y)
  z$model <- gsub("_engines$", "", x)
  z$object <- x
  z <- z[z$values,]
  z <- z[z$mode != "unknown",]
  z$values <- NULL
  names(z)[1] <- "engine"
  z$engine <- as.character(z$engine)
  z
}

engine_info <-
  purrr::map_df(
  parsnip:::engine_objects,
  get_engine_info
)

rm(engine_objects)

## nocov end
