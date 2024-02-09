test_that("utils-schema_from_history-history_creation", {
  expect_mapequal(list("x" = c("1", "2")), schema_from_history(list("00" = list("x" = "1"), "01" = list("x" = "2"))))
  expect_mapequal(list("x" = c("2", "1")), schema_from_history(list("01" = list("x" = "1"), "00" = list("x" = "2"))))
})
test_that("utils-schema_from_history-data_types", {
  expect_mapequal(list("x" = c("A", "2")), schema_from_history(list("00" = list("x" = "A"), "01" = list("x" = 2))))
})
test_that("utils-schema_from_history-various_nulls", {
  expect_mapequal(list("x" = c("A")), schema_from_history(list("00" = list("x" = "A"), "01" = list("x" = NULL))))
  expect_mapequal(list("x" = c("A")), schema_from_history(list("00" = list("x" = "A"), "01" = list("x" = integer(0)))))
  expect_mapequal(list("x" = c("A")), schema_from_history(list("00" = list("x" = "A"), "01" = list("x" = character(0)))))
})
test_that("utils-schema_from_history-vectors", {
  expect_mapequal(list("x" = c("A", "1", "2")), schema_from_history(list("00" = list("x" = "A"), "01" = list("x" = c(1, 2)))))
  expect_mapequal(list("x" = c("A", "2", "1")), schema_from_history(list("00" = list("x" = "A"), "01" = list("x" = c(2, 1)))))
  expect_mapequal(list("x" = c("A", "1", "2", "3")), schema_from_history(list("00" = list("x" = "A"), "01" = list("x" = c(1, 2, 3)))))
})
