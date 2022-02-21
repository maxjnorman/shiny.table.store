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
  schema <- shiny.table.store::table_schema(
    get_data = get_data
  )
  observe({
    tbl <- get_data()
    schema <- schema$get_schema()
    browser()
    print(tbl)
  })
}

shinyApp(
  ui = ui,
  server = server
)
