cat_lists <- function(list1, list2) {
  keys <- unique(c(names(list1), names(list2)))
  magrittr::set_names(purrr::map2(list1[keys], list2[keys], c), keys)
}
schema_from_tbl <- function(tbl) {
  logger::log_trace("call shiny.table.store::schema_from_tbl")
  schema <- lapply(tbl, unique)
  logger::log_trace("return shiny.table.store::schema_from_tbl")
  return(schema)
}
