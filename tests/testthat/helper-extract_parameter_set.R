check_parameter_set_tibble <- function(x) {
  expect_equal(
    names(x),
    c("name", "id", "source", "component", "component_id", "object")
  )
  expect_type(x$name, "character")
  expect_type(x$id, "character")
  expect_type(x$source, "character")
  expect_type(x$component, "character")
  expect_type(x$component_id, "character")
  expect_true(!anyDuplicated(x$id) > 0)

  expect_type(x$object, "list")
  obj_check <- purrr::map_lgl(x$object, \(x) {
    inherits(x, "param") | all(is.na(x))
  })
  expect_true(all(obj_check))

  invisible(TRUE)
}
