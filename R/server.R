#' @export
table_store <- function(input,
                        output,
                        session,
                        data = tibble::tibble(),
                        keys_ignore = c("value")
                        ) {
  history <- shiny::reactiveValues()
  history[[get_timestamp(i = "0")]] <- data
  get_data <- shiny::reactive(
    {
      history <- shiny::reactiveValuesToList(history)
      history <- history[order(names(history))]
      keys <- setdiff(purrr::reduce(lapply(history, names), union), keys_ignore)
      tbl <- purrr::reduce(history, unique_tbl, by = keys)
      return(tbl)
    }
  )
  return(list(
    "get_data" = get_data,
    "history" = history
  ))
}

#' @export
table_schema <- function(input,
                         output,
                         session,
                         get_data = shiny::reactive,
                         schema = list()) {
  history <- shiny::reactiveValues()
  history[[get_timestamp(i = "0")]] <- schema
  shiny::observeEvent(
    get_data(),
    {
      schema_tbl <- lapply(get_data(), unique)
      schema <- get_schema()
      if (is.null(names(schema))) {
        update <- schema_tbl
      } else {
        update <- purrr::map2(schema_tbl, schema, setdiff)
      }
      if (any(sapply(update, has_length))) {
        idx <- as.character(length(shiny::reactiveValuesToList(history)))
        history[[get_timestamp(i = idx)]] <- update
      }
    }
  )
  get_schema <- shiny::reactive(
    {
      history <- shiny::reactiveValuesToList(history)
      history <- history[order(names(history))]
      schema <- purrr::reduce(history, cat_lists)
      return(schema)
    }
  )
  apply_schema <- shiny::reactive(
    {
      data <- purrr::map2_df(get_data(), get_schema(), factor)
      return(data)
    }
  )
  return(list(
    "get_data" = apply_schema,
    "get_schema" = get_schema,
    "history" = history
  ))
}
