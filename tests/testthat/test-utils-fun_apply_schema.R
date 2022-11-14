test_that("utils-fun_apply_schema-empty_keys_ignore", {
  data <- tibble::tibble("x" = as.character(1:3))
  schema <- list(x = as.character(9:1))
  out <- tibble::tibble("x" = factor(1:3, levels = 9:1))
  expect_out <- purrr::partial(expect_identical, expected = out)
  make_out <- purrr::partial(fun_apply_schema, data = data, schema = schema)
  expect_out(make_out(keys_ignore = NULL))
  expect_out(make_out(keys_ignore = c("")))
  expect_out(make_out(keys_ignore = FALSE))
})

# test_that("utils-fun_apply_schema-", {
#   data <- tibble::tibble("x" = as.character(1:3), "y" = as.character(4:6))
#   schema <- list(x = as.character(9:1), y = as.character(9:1))
#   out <-
# })
