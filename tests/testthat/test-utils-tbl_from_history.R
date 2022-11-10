test_that("utils-tbl_from_history-only_lists_allowed", {
  A <- tibble::tibble(x = 1:3)
  B <- tibble::tibble(x = 2:4)
  # how to pass a single data.frame
  expect_no_error(tbl_from_history(list("A" = A)))
  expect_no_error(tbl_from_history(list(A)))
  expect_error(tbl_from_history(A))
  # how to pass a multiple data.frames
  expect_no_error(tbl_from_history(list("A" = A, "B" = B)))
  expect_no_error(tbl_from_history(list(A, B)))
  expect_error(tbl_from_history(c(A, B)))
  # how to pass copies of a data frame
  expect_no_error(tbl_from_history(list("A" = A, "A" = A)))
  expect_no_error(tbl_from_history(list(A, A)))
  expect_error(tbl_from_history(c(A, A)))
})
test_that("utils-tbl_from_history-simple_combo", {
  A <- tibble::tibble(x = 1:3)
  B <- tibble::tibble(x = 2:4)
  sorted_output <- tibble::tibble(x = 1:4) # sorted output
  unsorted_output <- tibble::tibble(x = c(2:4, 1)) # unsorted output
  # named lists should sort by name order
  expect_mapequal(sorted_output, tbl_from_history(list("A" = A, "B" = B)))
  expect_mapequal(sorted_output, tbl_from_history(list("B" = B, "A" = A)))
  # unnamed lists do not sort
  expect_mapequal(sorted_output, tbl_from_history(list(A, B)))
  expect_mapequal(unsorted_output, tbl_from_history(list(B, A)))
})
