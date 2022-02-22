# @export
table_store <- function(input,
                        output,
                        session,
                        data = tibble::tibble()) {
  get_data <- shiny::reactive(
    # implement!
    NULL
  )
  return(list(
    "get_data" = get_data
  ))
}

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
      schema_tbl <- schema_from_tbl(get_data())
      schema <- get_schema()
      if (is.null(names(schema))) {
        update <- schema_tbl
      } else {
        update <- purrr::map2(schema_tbl, schema, setdiff)
      }
      idx <- as.character(length(shiny::reactiveValuesToList(history)))
      history[[get_timestamp(i = idx)]] <- update
    }
  )
  get_schema <- reactive(
    {
      history <- shiny::reactiveValuesToList(history)
      history <- history[order(names(history))]
      schema <- purrr::reduce(history, cat_lists)
      return(schema)
    }
  )
  apply_schema <- reactive(
    {
      # implement!
      NULL
    }
  )
  return(list(
    "get_data" = apply_schema,
    "get_schema" = get_schema,
    "history" = history
  ))
}
