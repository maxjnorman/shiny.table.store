library(shiny)

# logger threshold choices are...
# logger::TRACE
# logger::DEBUG
# logger::INFO
# ?? logger::SUCCESS ??
# logger::WARN
# logger::ERROR
log_level <- logger::TRACE
logger::log_threshold(log_level)
logger::log_info("current log level is {log_level}:{attr(log_level, 'level')}")


ui <- fluidPage(
  column(
    width = 4,
    h2("Full data set"),
    tableOutput(outputId = "data_tbl"),
  ),
  column(
    width = 4,
    h2("Filter controls"),
    filter_core_ui(id = "filter_core")
  ),
  column(
    width = 4,
    h2("Filtered data"),
    tableOutput(outputId = "filter_tbl"),
  )
)
server <- function(input, output, session) {
  get_data <- reactive({
    invalidateLater(millis = 5)
    tbl <- tibble::tibble(
      x = as.character(round(runif(1, 1, 7))),
      y = as.character(round(runif(1, 3, 10))),
      value = rnorm(1, 0, 1)
    )
    return(tbl)
  })
  dat <- shiny::callModule(
    table_schema,
    id = "table_schema",
    data = tibble::tibble(x = character(), y = character(), value = double(0)),
    schema = list(),
    keys_ignore = c("value")
  )
  flt <- shiny::callModule(
    filter_core,
    id = "filter_core",
    get_data = dat$get_data,
    cols = c("x", "y"),
    labels = function(col) paste("FLT:", col)
  )
  observeEvent(
    get_data(),
    {
      if (nrow(dat$get_data()) < 15) {
        dat$set_update(get_data())
      }
    }
  )
  observeEvent(flt$get_data(), {
    print(flt$get_data())
  })
  output[["data_tbl"]] <- renderTable(
    dplyr::arrange(
      dat$get_data(),
      dplyr::across(
        dplyr::any_of(c("x", "y")),
        purrr::compose(as.double, as.character)
      )
    )
  )
  output[["filter_tbl"]] <- renderTable({
    flt$get_data()
  })
}

shinyApp(
  ui = ui,
  server = server
)
