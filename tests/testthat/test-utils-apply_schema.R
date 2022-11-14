test_that("utils-apply_schema-skip_keys_ignore", {
  data <- tibble::tibble("x" = as.character(1:3))
  schema <- list("x" = as.character(9:1))
  out <- tibble::tibble("x" = factor(1:3, levels = 9:1))
  expect_out <- purrr::partial(expect_identical, expected = out)
  make_obj <- purrr::partial(apply_schema, data = data, schema = schema)
  # keys_ignroe which should be wholly ignored
  expect_out(make_obj(keys_ignore = NULL))
  expect_out(make_obj(keys_ignore = FALSE))
  expect_out(make_obj(keys_ignore = c("")))
  # keys_ignore which are not in the data
  expect_out(make_obj(keys_ignore = c("y")))
  expect_out(make_obj(keys_ignore = c("y", "z")))
})

test_that("utils-apply_schema-full_keys_ignore", {
  data <- tibble::tibble("x" = as.character(1:3), "y" = as.character(4:6))
  schema <- list("x" = as.character(9:1), "y" = as.character(9:1))
  expect_data <- purrr::partial(expect_identical, expected = data)
  make_obj <- purrr::partial(apply_schema, data = data, schema = schema)
  expect_data(make_obj(keys_ignore = c("x", "y")))
})

test_that("utils-apply_schema-valid_keys_ignore", {
  data <- tibble::tibble("x" = as.character(1:3), "y" = as.character(4:6))
  schema <- list("x" = as.character(9:1), "y" = as.character(9:1))
  outX <- tibble::tibble("x" = factor(1:3, levels = 9:1), "y" = as.character(4:6))
  outY <- tibble::tibble("y" = factor(4:6, levels = 9:1), "x" = as.character(1:3))
  expect_identical(apply_schema(data, schema, keys_ignore = c("y")), outX)
  expect_identical(apply_schema(data, schema, keys_ignore = c("x")), outY)
})

test_that("utils-apply_schema-missing_schema_values_to_NA_in_data", {
  data <- tibble::tibble("x" = as.character(1:3), "y" = as.character(4:6))
  schema <- list("x" = seq(11, 1, -2), "y" = seq(2, 12, 2))
  expected <- tibble::tibble(
    x = factor(c("1", "NA", "3"), levels = schema[["x"]]),
    y = factor(c("4", "NA", "6"), levels = schema[["y"]])
  )
  expect_identical(apply_schema(data, schema, keys_ignore = NULL), expected)
})
