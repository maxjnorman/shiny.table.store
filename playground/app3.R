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
    time_rev <- paste(rev(stringr::str_split(Sys.time(), "")[[1]]), collapse = "")
    dat <- stringr::str_split(time_rev, " ")
    tbl <- tibble::tibble(x = dat[[1]][[1]], y = dat[[1]][[2]], value = rnorm(1, 0, 1))
    return(tbl)
  })
  data <- shiny::callModule(
    table_schema,
    id = "testy-test",
    data = tibble::tibble(x = character(), y = character(), value = double(0)),
    schema = list(),
    keys_ignore = c("value")
  )
  observeEvent(
    get_data(),
    data$set_update(get_data())
  )
  observeEvent(data$get_schema(), {
    print(data$get_data())
    print(data$get_schema())
    browser()
  })
}

shinyApp(
  ui = ui,
  server = server
)
