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

cat_lists <- function(list1, list2, keys = NULL) {
  logger::log_trace("call shiny.table.store::cat_lists")
  # https://stackoverflow.com/questions/18538977/combine-merge-lists-by-elements-names
  if (is.null(keys)) keys <- unique(c(names(list1), names(list2)))
  lists <- magrittr::set_names(purrr::map2(list1[keys], list2[keys], c), keys)
  logger::log_trace("return shiny.table.store::cat_lists")
  return(lists)
}

has_length <- purrr::compose(as.logical, length)
has_rows <- purrr::compose(as.logical, nrow)
req_length <- purrr::compose(shiny::req, has_length)
req_rows <- purrr::compose(shiny::req, has_rows)
not_truthy <- purrr::compose(magrittr::not, shiny::isTruthy)
has_names <- purrr::compose(magrittr::not, is.null, names)

unique_tbl <- function(left, right, ...) {
  logger::log_trace("call shiny.table.store::unique_tbl")
  stopifnot(is(left, "data.frame")) # a tibble::tibble() is OK too
  stopifnot(is(right, "data.frame"))
  tbl <- dplyr::bind_rows(left, dplyr::anti_join(right, left, ...))
  logger::log_trace("return shiny.table.store::unique_tbl")
  return(tbl)
}

ifthen <- function(obj, then, test = not_truthy) {
  logger::log_trace("call shiny.table.store::ifthen")
  if (isTRUE(test(obj))) {
    output <- then
  } else {
    output <- obj
  }
  logger::log_trace("return shiny.table.store::ifthen")
  return(output)
}

sort_by_name <- function(list) {
  logger::log_trace("call shiny.table.store::sort_by_name")
  list <- list[order(names(list))]
  logger::log_trace("return shiny.table.store::sort_by_name")
  return(list)
}

get_common_keys <- function(history, keys_ignore = NULL) {
  logger::log_trace("call shiny.table.store::get_common_keys")
  keys <- lapply(history, names)
  keys <- purrr::reduce(keys, intersect)
  keys <- setdiff(keys, keys_ignore)
    logger::log_trace("return shiny.table.store::get_common_keys")
  return(keys)
}

tbl_from_history <- function(history, keys_ignore = NULL) {
  logger::log_trace("call shiny.table.store::tbl_from_history")
  stopifnot(is(history, "list"))
  if (has_names(history)) history <- sort_by_name(history)
  keys <- get_common_keys(history, keys_ignore = keys_ignore)
  tbl <- purrr::reduce(history, unique_tbl, by = keys)
  logger::log_trace("return shiny.table.store::tbl_from_history")
  return(tbl)
}

schema_from_history <- function(history) {
  logger::log_trace("call shiny.table.store::schema_from_history")
  if (has_names(history)) history <- sort_by_name(history)
  schema <- purrr::reduce(history, cat_lists)
  logger::log_trace("return shiny.table.store::schema_from_history")
  return(schema)
}

make_labels_from_cols <- function(cols, labels) {
  logger::log_trace("call shiny.table.store::make_labels_from_cols")
  if (is.null(labels)) {
    labels <- cols
  } else if (is(labels, "function")) { # if a function is passed
    labels <- labels(cols)
  } else if (length(labels) == 1) { # if a single value is passed
    labels <- rep(labels, length(cols))
  } # else just use the labels that were provided as-is
  stopifnot(length(labels) == length(cols))
  logger::log_trace("return shiny.table.store::make_labels_from_cols")
  return(labels)
}

fun_apply_schema <- function(data, schema, keys_ignore) {
  logger::log_trace("call shiny.table.store::fun_apply_schema")
  common_keys <- get_common_keys(list(data, schema), keys_ignore)
  keys_tbl <- purrr::map2_df(data[common_keys], schema[common_keys], factor)
  keys_ignore <- intersect(keys_ignore, colnames(data))
  keys_ignore <- ifthen(keys_ignore, test = not_truthy, then = NULL)
  ignore_tbl <- data[keys_ignore]
  out <- dplyr::bind_cols(keys_tbl, ignore_tbl)
  logger::log_trace("return shiny.table.store::fun_apply_schema")
  return(out)
}
