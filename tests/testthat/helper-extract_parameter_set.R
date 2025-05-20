check_parameter_set_tibble <- function(x) {
  expect_equal(names(x), c("name", "id", "source", "component", "component_id", "object"))
  expect_equal(class(x$name), "character")
  expect_equal(class(x$id), "character")
  expect_equal(class(x$source), "character")
  expect_equal(class(x$component), "character")
  expect_equal(class(x$component_id), "character")
  expect_true(!any(duplicated(x$id)))

  expect_equal(class(x$object), "list")
  obj_check <- purrr::map_lgl(x$object, \(x) inherits(x, "param") | all(is.na(x)))
  expect_true(all(obj_check))

  invisible(TRUE)
}
