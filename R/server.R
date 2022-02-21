# @export
table_store <- function(
  input,
  output,
  session,
  data = tibble::tibble()
) {

  get_data <- shiny::reactive(
    # implement!
    NULL
  )

  return(list(
    "get_data" = get_data
  ))
}

table_schema <- function(
  input,
  output,
  session,
  get_data = shiny::reactive,
  schema = list()
) {
  history <- reactiveValues()
  shiny::observeEvent(
    get_data(),
    {
      idx <- length(history) + 1
      update <- Map(c, get_schema(), schema_from_tbl(get_data()))
      history[[idx]] <- update
    }
  )
  get_schema <- reactive(
    {
      history <- reactiveValuesToList(history)
      history <- history[order(names(history))] # listory..?
    }
  )

  return(list(
    "get_schema" = get_schema
  ))
}
