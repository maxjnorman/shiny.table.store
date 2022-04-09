get_timestamp_true <- function() {
  logger::log_trace("call shiny.table.store::get_timestamp_true")
  timestamp <- str_remove_all(Sys.time(), "[^0-9]")
  logger::log_trace("return shiny.table.store::get_timestamp_true")
  return(timestamp)
}
get_timestamp <- function(i = NULL) {
  logger::log_trace("call shiny.table.store::get_timestamp")
  if (is.null(i)) {
    timestamp <- get_timestamp_true()
  } else {
    stopifnot(i >= 0)
    timestamp <- stringr::str_pad(i, 14, "left", "0")
  }
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
schema_from_tbl <- function(tbl) {
  logger::log_trace("call shiny.table.store::schema_from_tbl")
  schema <- lapply(tbl, unique)
  logger::log_trace("return shiny.table.store::schema_from_tbl")
  return(schema)
}
has_length <- purrr::compose(as.logical, length)
unique_tbl <- function(left, right, ...) {
  tbl <- dplyr::bind_rows(left, dplyr::anti_join(right, left, ...))
  return(tbl)
}
