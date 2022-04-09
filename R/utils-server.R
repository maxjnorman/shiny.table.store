get_timestamp <- function(i = NULL, size = as.integer(14)) {
  logger::log_trace("call shiny.table.store::get_timestamp")
  stopifnot(is.integer(size))
  stopifnot(size >= 14)
  if (is.null(i)) {
    i <- stringr::str_remove_all(Sys.time(), "[^0-9]")
  } else {
    stopifnot(is.integer(i))
    stopifnot(i >= 0)
    i <- as.character(i)
  }
  timestamp <- stringr::str_pad(i, size, "left", "0")
  logger::log_trace("return shiny.table.store::get_timestamp")
  return(timestamp)
}
cat_lists <- function(list1, list2) {
  logger::log_trace("call shiny.table.store::cat_lists")
  # https://stackoverflow.com/questions/18538977/combine-merge-lists-by-elements-names
  keys <- unique(c(names(list1), names(list2)))
  lists <- magrittr::set_names(purrr::map2(list1[keys], list2[keys], c), keys)
  logger::log_trace("return shiny.table.store::cat_lists")
  return(lists)
}
has_length <- purrr::compose(as.logical, length)
unique_tbl <- function(left, right, ...) {
  tbl <- dplyr::bind_rows(left, dplyr::anti_join(right, left, ...))
  return(tbl)
}
