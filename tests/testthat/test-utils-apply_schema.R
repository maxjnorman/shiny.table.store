test_that("utils-apply_schema-skip_keys_ignore", {
  # set up the inputs and output for the following tests
  data <- tibble::tibble("x" = as.character(1:3)) # the raw values for the output tibble
  schema <- list("x" = as.character(9:1)) # the schema will become the levels of the output
  out <- tibble::tibble("x" = factor(1:3, levels = 9:1))
  # use partial to set the variables that won't change within this test (data, schema)
  make_obj <- purrr::partial(apply_schema, data = data, schema = schema)
  # keys_ignore in the below should be wholly ignored becuase they are not valid keys/tibble headers
  expect_identical(make_obj(keys_ignore = NULL), expected = out)
  expect_identical(make_obj(keys_ignore = FALSE), expected = out)
  expect_identical(make_obj(keys_ignore = c("")), expected = out)
  # keys_ignore which are not in the data - only c("x") is a valid key in data
  expect_identical(make_obj(keys_ignore = c("y")), expected = out)
  expect_identical(make_obj(keys_ignore = c("y", "z")), expected = out)
})

test_that("utils-apply_schema-full_keys_ignore", {
  data <- tibble::tibble("x" = as.character(1:3), "y" = as.character(4:6))
  schema <- list("x" = as.character(9:1), "y" = as.character(9:1))
  # use partial to set the variables that won't change within this test (data, schema)
  make_obj <- purrr::partial(apply_schema, data = data, schema = schema)
  # expect the original data set returned unchanged
  expect_identical(make_obj(keys_ignore = c("x", "y")), expected = data) # ignore both x and y
  expect_identical(make_obj(keys_ignore = c("x", "y", "z")), expected = data) # keys which are not in the data ("z") can be ignored anyway 
})

test_that("utils-apply_schema-valid_keys_ignore", {
  data <- tibble::tibble("x" = as.character(1:3), "y" = as.character(4:6))
  schema <- list("x" = as.character(9:1), "y" = as.character(9:1))
  outX <- tibble::tibble("x" = factor(1:3, levels = 9:1), "y" = as.character(4:6))
  outY <- tibble::tibble("y" = factor(4:6, levels = 9:1), "x" = as.character(1:3))
  expect_identical(apply_schema(data, schema, keys_ignore = c("y")), outX)
  expect_identical(apply_schema(data, schema, keys_ignore = c("x")), outY)
})

test_that("utils-apply_schema-missing_schema_items_returned_unchanged", {
  data <- tibble::tibble("x" = as.character(1:3), "y" = as.character(4:6))
  schema <- list("x" = as.character(seq(1, 11, 2)), "y" = as.character(seq(2, 12, 2)))
  fct_x <- factor(c("1", "NA", "3"), levels = schema[["x"]])
  fct_y <- factor(c("4", "NA", "6"), levels = schema[["y"]])
  expected_x <- tibble::tibble("x" = fct_x, "y" = data[["y"]])
  expected_y <- tibble::tibble("y" = fct_y, "x" = data[["x"]])
  expect_identical(apply_schema(data, schema["x"], keys_ignore = NULL), expected_x)
  expect_identical(apply_schema(data, schema["y"], keys_ignore = NULL), expected_y)
})

test_that("utils-apply_schema-missing_schema_values_to_NA_in_data", {
  data <- tibble::tibble("x" = as.character(1:3), "y" = as.character(4:6))
  schema <- list("x" = as.character(seq(1, 11, 2)), "y" = as.character(seq(2, 12, 2)))
  expected <- tibble::tibble(
    x = factor(c("1", "NA", "3"), levels = schema[["x"]]),
    y = factor(c("4", "NA", "6"), levels = schema[["y"]])
  )
  expect_identical(apply_schema(data, schema, keys_ignore = NULL), expected)
})
