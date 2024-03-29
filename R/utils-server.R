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

#' Function to determine if an R object has length
#'
#' @export
#' @name has_length
#' @param x an R object. For replacement, a vector or factor.
#' @return a non-negative integer or double (which will be rounded down).
has_length <- purrr::compose(as.logical, length)

has_rows <- purrr::compose(as.logical, nrow)
istrue_rows <- purrr::compose(isTRUE, has_rows)
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

#' Apply a schema to data
#'
#' @param data A tibble::tibble contianing values.
#' @param schema A named list of character vectors. Each vector is matched to a column in 'data' and transforms it into a factor.
#' @param keys_ignore A character vector or NULL, these columns are not transformed.
#' @returns A tibble::tibble.
#' @export
apply_schema <- function(data, schema, keys_ignore = NULL) {
  logger::log_trace("call shiny.table.store::apply_schema")
  keys_common <- get_common_keys(list(data, schema), keys_ignore)
  keys_missed <- setdiff(names(data), keys_common)
  keys_ignore <- unique(c(keys_ignore, keys_missed))
  keys_ignore <- intersect(keys_ignore, colnames(data))
  keys_ignore <- ifthen(keys_ignore, test = not_truthy, then = NULL)
  tbl_keys <- purrr::map2_df(data[keys_common], schema[keys_common], factor)
  tbl_ignore <- data[keys_ignore]
  if (istrue_rows(tbl_keys) & istrue_rows(tbl_ignore)) {
    out <- dplyr::bind_cols(tbl_keys, tbl_ignore)
  } else if (istrue_rows(tbl_keys)) {
    out <- tbl_keys
  } else {
    out <- tbl_ignore
  }
  logger::log_trace("return shiny.table.store::apply_schema")
  return(out)
}
