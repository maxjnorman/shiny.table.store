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
  filter_core_ui(id = "test-filter_core")
)
server <- function(input, output, session) {
  get_data <- reactive({
    invalidateLater(millis = 50)
    time_rev <- paste(rev(stringr::str_split(Sys.time(), "")[[1]]), collapse = "")
    dat <- stringr::str_split(time_rev, " ")
    tbl <- tibble::tibble(x = dat[[1]][[1]], y = dat[[1]][[2]], value = rnorm(1, 0, 1))
    return(tbl)
  })
  dat <- shiny::callModule(
    table_schema,
    id = "test-table_schema",
    data = tibble::tibble(x = character(), y = character(), value = double(0)),
    schema = list(),
    keys_ignore = c("value")
  )
  flt <- shiny::callModule(
    filter_core,
    id = "test-filter_core",
    get_data = dat$get_data,
    cols = c("x", "y")
  )
  observeEvent(
    get_data(),
    {
      if (nrow(dat$get_data()) < 5) {
        dat$set_update(get_data())
      }
    }
  )
  observeEvent(flt$get_data(), {
    print(flt$get_data())
    browser()
  })
}

shinyApp(
  ui = ui,
  server = server
)
