test_that("utils-cat_lists-single_name", {
  expect_mapequal(list("A" = 1), cat_lists(list("A" = 1), list()))
  expect_mapequal(list("A" = 1), cat_lists(list(), list("A" = 1)))
  expect_mapequal(list("A" = c(1, 1)), cat_lists(list("A" = 1), list("A" = 1)))
  expect_mapequal(list("A" = c(1, 2)), cat_lists(list("A" = 1), list("A" = 2)))
  expect_mapequal(list("A" = c(2, 1)), cat_lists(list("A" = 2), list("A" = 1)))
})
test_that("utils-cat_lists-two_names", {
  expect_mapequal(list("A" = 1, "B" = 1), cat_lists(list("A" = 1), list("B" = 1)))
  expect_mapequal(list("A" = 1, "B" = c(1, 2)), cat_lists(list("A" = 1, "B" = 1), list("B" = 2)))
  expect_mapequal(list("A" = 1, "B" = c(1, 2)), cat_lists(list("A" = 1), list("B" = c(1, 2))))
})
test_that("utils-cat_lists-null_elements", {
  expect_mapequal(list("A" = 1), cat_lists(list("A" = 1), list("A" = NULL)))
  expect_mapequal(list("A" = 1), cat_lists(list("A" = NULL), list("A" = 1)))
  expect_mapequal(list("A" = 1, "B" = NULL), cat_lists(list("A" = 1), list("B" = NULL)))
  expect_mapequal(list("A" = NULL, "B" = 1), cat_lists(list("A" = NULL), list("B" = 1)))
  expect_mapequal(list("A" = NULL, "B" = NULL), cat_lists(list("A" = NULL), list("B" = NULL)))
})
