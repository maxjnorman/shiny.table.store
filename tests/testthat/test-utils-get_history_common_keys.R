test_that("utils-get_history_common_keys-single_key", {
  tbl_A <- tibble::tibble("x" = as.character(1:3))
  tbl_B <- tibble::tibble("x" = as.integer(4:6))
  lst_A <- list("x" = as.character(1:2))
  lst_B <- list("x" = as.integer(3:6))
  keys <- "x"
  expect_equal(keys, get_history_common_keys(list(tbl_A, tbl_B)))
  expect_equal(keys, get_history_common_keys(list(lst_A, lst_B)))
})
test_that("utils-get_history_common_keys-single_valid_key", {
  tbl_A <- tibble::tibble("x" = as.character(1:3), "y" = as.character(4:6))
  tbl_B <- tibble::tibble("x" = as.integer(4:6), "z" = as.integer(1:3))
  lst_A <- list("x" = as.character(1:2), "y" = as.character(3:6))
  lst_B <- list("x" = as.integer(5:6), "z" = as.integer(1:4))
  keys <- "x"
  expect_equal(keys, get_history_common_keys(list(tbl_A, tbl_B)))
  expect_equal(keys, get_history_common_keys(list(lst_A, lst_B)))
})
test_that("utils-get_history_common_keys-ignore_valid_key", {
  tbl_A <- tibble::tibble("x" = as.character(1:3), "y" = as.character(4:6))
  tbl_B <- tibble::tibble("x" = as.integer(4:6), "z" = as.integer(1:3))
  lst_A <- list("x" = as.character(1:2), "y" = as.character(3:6))
  lst_B <- list("x" = as.integer(5:6), "z" = as.integer(1:4))
  keys <- "x"
  expect_equal(character(0), get_history_common_keys(list(tbl_A, tbl_B), keys_ignore = keys))
  expect_equal(character(0), get_history_common_keys(list(lst_A, lst_B), keys_ignore = keys))
})
