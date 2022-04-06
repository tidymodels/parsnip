# There's currently an issue comparing tibbles so we do it col-by-col
test_by_col <- function(a, b) {
  for (i in union(names(a), names(b))) {
    expect_equal(a[[i]], b[[i]])
  }
}

# ------------------------------------------------------------------------------

test_that('adding a new model', {
  set_new_model("sponge")

  mod_items <- get_model_env() %>% rlang::env_names()
  sponges <- grep("sponge", mod_items, value = TRUE)
  exp_obj <- c('sponge_modes', 'sponge_fit', 'sponge_args',
               'sponge_predict', 'sponge_pkgs', 'sponge')
  expect_equal(sort(sponges), sort(exp_obj))

  expect_equal(
    get_from_env("sponge"),
    tibble(engine = character(0), mode = character(0))
  )

test_by_col(
  get_from_env("sponge_pkgs"),
  tibble(engine = character(0), pkg = list(), mode = character(0))
)

expect_equal(
  get_from_env("sponge_modes"), "unknown"
)

test_by_col(
  get_from_env("sponge_args"),
  dplyr::tibble(engine = character(0), parsnip = character(0),
                original = character(0), func = vector("list"),
                has_submodel = logical(0))
)

test_by_col(
  get_from_env("sponge_fit"),
  tibble(engine = character(0), mode = character(0), value = vector("list"))
)

test_by_col(
  get_from_env("sponge_predict"),
  tibble(engine = character(0), mode = character(0),
         type = character(0), value = vector("list"))
)

expect_error(set_new_model())
expect_error(set_new_model(2))
expect_error(set_new_model(letters[1:2]))
})


# ------------------------------------------------------------------------------

test_that('adding a new mode', {
  set_model_mode("sponge", "classification")

  expect_equal(get_from_env("sponge_modes"), c("unknown", "classification"))

  expect_error(set_model_mode("sponge"))

})


# ------------------------------------------------------------------------------

test_that('adding a new engine', {
  set_model_engine("sponge", mode = "classification", eng = "gum")

  test_by_col(
    get_from_env("sponge"),
    tibble(engine = "gum", mode = "classification")
  )

  expect_equal(get_from_env("sponge_modes"), c("unknown", "classification"))

  expect_error(set_model_engine("sponge", eng = "gum"))
  expect_error(set_model_engine("sponge", mode = "classification"))
  expect_error(
    set_model_engine("sponge", mode = "regression", eng = "gum"),
    "'regression' is not a known mode"
  )
})


# ------------------------------------------------------------------------------

test_that('adding a new package', {
  set_dependency("sponge", "gum", "trident")

  expect_error(set_dependency("sponge", "gum", letters[1:2]))
  expect_error(set_dependency("sponge", "gummies", "trident"))
  expect_error(set_dependency("sponge",  "gum", "trident", mode = "regression"))

  test_by_col(
    get_from_env("sponge_pkgs"),
    tibble(engine = "gum", pkg = list("trident"), mode = "classification")
  )

  set_dependency("sponge", "gum", "juicy-fruit", mode = "classification")
  test_by_col(
    get_from_env("sponge_pkgs"),
    tibble(engine = "gum",
           pkg = list(c("trident", "juicy-fruit")),
           mode = "classification")
  )

  test_by_col(
    get_dependency("sponge"),
    tibble(engine = "gum",
           pkg = list(c("trident", "juicy-fruit")),
           mode = "classification")
  )
})


# ------------------------------------------------------------------------------

test_that('adding a new argument', {
  set_model_arg(
    model = "sponge",
    eng = "gum",
    parsnip = "modeling",
    original = "modelling",
    func = list(pkg = "foo", fun = "bar"),
    has_submodel = FALSE
  )

  set_model_arg(
    model = "sponge",
    eng = "gum",
    parsnip = "modeling",
    original = "modelling",
    func = list(pkg = "foo", fun = "bar"),
    has_submodel = FALSE
  )

  args <- get_from_env("sponge_args")
  expect_equal(sum(args$parsnip == "modeling"), 1)

  test_by_col(
    get_from_env("sponge_args"),
    tibble(engine = "gum", parsnip = "modeling", original = "modelling",
           func = list(list(pkg = "foo", fun = "bar")),
           has_submodel = FALSE)
  )

  expect_error(
    set_model_arg(
      model = "lunchroom",
      eng = "gum",
      parsnip = "modeling",
      original = "modelling",
      func = list(pkg = "foo", fun = "bar"),
      has_submodel = FALSE
    )
  )

  expect_error(
    set_model_arg(
      model = "sponge",
      eng = "gum",
      parsnip = "modeling",
      func = list(pkg = "foo", fun = "bar"),
      has_submodel = FALSE
    )
  )

  expect_error(
    set_model_arg(
      model = "sponge",
      eng = "gum",
      original = "modelling",
      func = list(pkg = "foo", fun = "bar"),
      has_submodel = FALSE
    )
  )

  expect_error(
    set_model_arg(
      model = "sponge",
      eng = "gum",
      parsnip = "modeling",
      original = "modelling",
      func = "foo::bar",
      has_submodel = FALSE
    )
  )

  expect_error(
    set_model_arg(
      model = "sponge",
      eng = "gum",
      parsnip = "modeling",
      original = "modelling",
      func = list(pkg = "foo", fun = "bar"),
      has_submodel = 2
    )
  )

  expect_error(
    set_model_arg(
      model = "sponge",
      eng = "gum",
      parsnip = "modeling",
      original = "modelling",
      func = list(pkg = "foo", fun = "bar")
    )
  )

  expect_error(
    set_model_arg(
      model = "sponge",
      eng = "gum",
      parsnip = "yodeling",
      original = "yodelling",
      func = c(foo = "a", bar = "b"),
      has_submodel = FALSE
    )
  )

  expect_error(
    set_model_arg(
      model = "sponge",
      eng = "gum",
      parsnip = "yodeling",
      original = "yodelling",
      func = c(foo = "a"),
      has_submodel = FALSE
    )
  )

  expect_error(
    set_model_arg(
      model = "sponge",
      eng = "gum",
      parsnip = "yodeling",
      original = "yodelling",
      func = c(fun = 2, pkg = 1),
      has_submodel = FALSE
    )
  )
})



# ------------------------------------------------------------------------------

test_that('adding a new fit', {
  fit_vals <-
    list(
      interface = "formula",
      protect = c("formula", "data"),
      func = c(pkg = "foo", fun = "bar"),
      defaults = list()
    )

  set_fit(
    model = "sponge",
    eng = "gum",
    mode = "classification",
    value = fit_vals
  )

  fit_env_data <- get_from_env("sponge_fit")
  test_by_col(
    fit_env_data[ 1:2],
    tibble(engine = "gum", mode = "classification")
  )

  expect_equal(
    fit_env_data$value[[1]],
    fit_vals
  )

  expect_error(
    set_fit(
      model = "cactus",
      eng = "gum",
      mode = "classification",
      value = fit_vals
    )
  )

  expect_error(
    set_fit(
      model = "sponge",
      eng = "nose",
      mode = "classification",
      value = fit_vals
    )
  )

  expect_error(
    set_fit(
      model = "sponge",
      eng = "gum",
      mode = "frog",
      value = fit_vals
    )
  )

  for (i in 1:length(fit_vals)) {
    expect_error(
      set_fit(
        model = "sponge",
        eng = "gum",
        mode = "classification",
        value = fit_vals[-i]
      )
    )
  }

  fit_vals_0 <- fit_vals
  fit_vals_0$interface <- "loaf"
  expect_error(
    set_fit(
      model = "sponge",
      eng = "gum",
      mode = "classification",
      value = fit_vals_0
    )
  )

  fit_vals_1 <- fit_vals
  fit_vals_1$defaults <- 2
  expect_error(
    set_fit(
      model = "sponge",
      eng = "gum",
      mode = "classification",
      value = fit_vals_1
    )
  )

  fit_vals_2 <- fit_vals
  fit_vals_2$func <- "foo:bar"
  expect_error(
    set_fit(
      model = "sponge",
      eng = "gum",
      mode = "classification",
      value = fit_vals_2
    )
  )

  fit_vals_3 <- fit_vals
  fit_vals_3$interface <- letters
  expect_error(
    set_fit(
      model = "sponge",
      eng = "gum",
      mode = "classification",
      value = fit_vals_3
    )
  )

  test_by_col(
    get_fit("sponge")[, 1:2],
    tibble(engine = "gum", mode = "classification")
  )

  expect_equal(
    get_fit("sponge")$value[[1]],
    fit_vals
  )
})


# ------------------------------------------------------------------------------

test_that('adding a new predict method', {
  class_vals <-
    list(
      pre = I,
      post = NULL,
      func = c(fun = "predict"),
      args = list(x = quote(2))
    )

  set_pred(
    model = "sponge",
    eng = "gum",
    mode = "classification",
    type = "class",
    value = class_vals
  )

  pred_env_data <- get_from_env("sponge_predict")
  test_by_col(
    pred_env_data[ 1:3],
    tibble(engine = "gum", mode = "classification", type = "class")
  )

  expect_equal(
    pred_env_data$value[[1]],
    class_vals
  )

  test_by_col(
    get_pred_type("sponge", "class")[ 1:3],
    tibble(engine = "gum", mode = "classification", type = "class")
  )

  expect_equal(
    get_pred_type("sponge", "class")$value[[1]],
    class_vals
  )

  expect_error(
    set_pred(
      model = "cactus",
      eng = "gum",
      mode = "classification",
      type = "class",
      value = class_vals
    )
  )

  expect_error(
    set_pred(
      model = "sponge",
      eng = "nose",
      mode = "classification",
      type = "class",
      value = class_vals
    )
  )


  expect_error(
    set_pred(
      model = "sponge",
      eng = "gum",
      mode = "classification",
      type = "eggs",
      value = class_vals
    )
  )

  expect_error(
    set_pred(
      model = "sponge",
      eng = "gum",
      mode = "frog",
      type = "class",
      value = class_vals
    )
  )

  for (i in 1:length(class_vals)) {
    expect_error(
      set_pred(
        model = "sponge",
        eng = "gum",
        mode = "classification",
        type = "class",
        value = class_vals[-i]
      )
    )
  }

  class_vals_0 <- class_vals
  class_vals_0$pre <- "I"
  expect_error(
    set_pred(
      model = "sponge",
      eng = "gum",
      mode = "classification",
      type = "class",
      value = class_vals_0
    )
  )

  class_vals_1 <- class_vals
  class_vals_1$post <- "I"
  expect_error(
    set_pred(
      model = "sponge",
      eng = "gum",
      mode = "classification",
      type = "class",
      value = class_vals_1
    )
  )

  class_vals_2 <- class_vals
  class_vals_2$func <- "foo:bar"
  expect_error(
    set_pred(
      model = "sponge",
      eng = "gum",
      mode = "classification",
      type = "class",
      value = class_vals_2
    )
  )

})



test_that('showing model info', {
  expect_output(
    show_model_info("rand_forest"),
    "Information for `rand_forest`"
  )
  expect_output(
    show_model_info("rand_forest"),
    "trees --> ntree"
  )
  expect_output(
    show_model_info("rand_forest"),
    "fit modules:"
  )
  expect_output(
    show_model_info("rand_forest"),
    "prediction modules:"
  )
})

