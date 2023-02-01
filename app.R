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
    tableOutput(outputId = "data_tbl")
  ),
  column(
    width = 4,
    actionButton(inputId = "add_row", label = "Add row"),
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
  dat <- shiny::callModule( # Full version of the table
    table_schema,
    id = "table_schema",
    data = tibble::tibble( # Start with an empty data frame
      x = character(),
      y = character(),
      value = double(0)
    ),
    schema = list(),
    keys_ignore = c("value")
  )
  get_data <- eventReactive( # Get a row of random data
    input[["add_row"]], # Watch the 'Add row' button
    tibble::tibble( # Single-row table of random values
      x = as.character(round(runif(1, 1, 7))),
      y = as.character(round(runif(1, 3, 10))),
      value = as.double(rnorm(1, 0, 1))
    )
  )
  observeEvent(get_data(), dat$set_update(get_data()))
  flt <- shiny::callModule(
    filter_core,
    id = "filter_core",
    get_data = dat$get_data, # Take in the full data
    cols = c("x", "y"), # Table columns to filter
    labels = function(col) paste("FLT:", col)
  )
  output[["data_tbl"]] <- renderTable(dat$get_data())
  output[["filter_tbl"]] <- renderTable(flt$get_data())
  observeEvent(flt$get_data(), print(flt$get_data()))
}

shinyApp(
  ui = ui,
  server = server,
  options = list("display.mode" = "showcase")
)
