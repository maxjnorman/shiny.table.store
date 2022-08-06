test_that("true-vector_integers", {
  expect_true(has_length(0:0))
  expect_true(has_length(0:1))
  expect_true(has_length(0:2))
  expect_true(has_length(0:3))
})
test_that("true-specials", {
  expect_true(has_length(FALSE))
  expect_true(has_length(TRUE))
  expect_true(has_length(as.double(NA)))
  expect_true(has_length(as.integer(NA)))
  expect_true(has_length(as.character(NA)))
})
test_that("false-specials", {
  expect_false(has_length(NULL))
  expect_false(has_length(character(0)))
  expect_false(has_length(numeric(0)))
  expect_false(has_length(integer(0)))
})
