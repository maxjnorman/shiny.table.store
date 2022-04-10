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


ui <- fluidPage()
server <- function(input, output, session) {
  get_data <- reactive({
    invalidateLater(millis = 500)
    dat <- stringr::str_split(Sys.time(), " ")
    tbl <- tibble::tibble(x = dat[[1]][[1]], y = dat[[1]][[2]])
    return(tbl)
  })
  data <- callModule(
    table_core,
    id = "testy-test",
    data = tibble::tibble(x = character(), y = character())
  )
  schema <- callModule(
    schema_core,
    id = "testy-test",
    get_data = data$get_data
  )
  observeEvent(get_data(), {
    idx <- length(reactiveValuesToList(data$history))
    data$history[[get_timestamp(idx)]] <- get_data()
  })
  observeEvent(schema$get_schema(), {
    print(data$get_data())
    print(schema$get_schema())
    print(schema$get_data())
    browser()
  })
}

shinyApp(
  ui = ui,
  server = server
)
