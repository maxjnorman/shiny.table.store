library(shiny)

ui <- fluidPage()
server <- function(input, output, session) {
  get_data <- reactive({
    invalidateLater(millis = 2350)
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