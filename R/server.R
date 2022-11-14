#' @export
table_core <- function(input,
                       output,
                       session,
                       data = tibble::tibble(),
                       keys_ignore = c("value")) {
  history <- shiny::reactiveValues()
  history[[get_timestamp(i = as.integer(0))]] <- data
  get_data <- shiny::reactive({
    history <- shiny::reactiveValuesToList(history)
    tbl <- tbl_from_history(history, keys_ignore = keys_ignore)
    return(tbl)
  })
  return(list(
    "get_data" = get_data,
    "history" = history
  ))
}

#' @export
table_store <- function(input,
                        output,
                        session,
                        data = tibble::tibble(),
                        keys_ignore = c("value")) {
  engine <- shiny::callModule(
    module = table_core,
    id = "engine",
    data = data,
    keys_ignore = keys_ignore
  )
  set_update <- shiny::reactiveVal()
  shiny::observeEvent(
    set_update(),
    {
      update <- set_update()
      data <- engine$get_data()
      keys <- get_history_common_keys(list(update, data), keys_ignore = keys_ignore)
      update <- dplyr::anti_join(update, data, by = keys)
      if (isTRUE(has_rows(update))) {
        idx <- length(shiny::reactiveValuesToList(engine$history))
        engine$history[[get_timestamp(i = idx)]] <- update
      }
    }
  )
  return(list(
    "get_data" = engine$get_data,
    "history" = engine$history,
    "set_update" = set_update
  ))
}

#' @export
schema_core <- function(input,
                        output,
                        session,
                        get_data = shiny::reactive,
                        schema = list(),
                        keys_ignore = c("value")) {
  history <- shiny::reactiveValues()
  history[[get_timestamp(i = as.integer(0))]] <- schema
  get_schema <- shiny::reactive({
    history <- shiny::reactiveValuesToList(history)
    schema <- schema_from_history(history)
    return(schema)
  })
  mod_apply_schema <- purrr::partial(fun_apply_schema, keys_ignore = keys_ignore)
  rct_apply_schema <- shiny::reactive(
    mod_apply_schema(data = get_data(), schema = get_schema())
  )
  shiny::observeEvent(
    get_data(),
    {
      data <- get_data()
      keys <- get_history_common_keys(list(data), keys_ignore = keys_ignore)
      schema <- get_schema()
      schema_tbl <- lapply(data[keys], unique)
      if (has_names(schema)) {
        update <- purrr::map2(schema_tbl, schema, setdiff)
      } else {
        update <- schema_tbl
      }
      if (any(sapply(update, has_length))) {
        idx <- length(shiny::reactiveValuesToList(history))
        history[[get_timestamp(i = idx)]] <- update
      }
    }
  )
  return(list(
    "get_data" = rct_apply_schema,
    "get_schema" = get_schema
  ))
}

#' @export
table_schema <- function(input,
                         output,
                         session,
                         data = tibble::tibble(),
                         schema = list(),
                         keys_ignore = c("value")) {
  engine <- callModule(
    module = table_store,
    id = "engine",
    data = data,
    keys_ignore = keys_ignore
  )
  schema <- callModule(
    module = schema_core,
    id = "schema",
    get_data = engine$get_data,
    schema = schema,
    keys_ignore = keys_ignore
  )
  return(list(
    "get_data" = schema$get_data,
    "get_schema" = schema$get_schema,
    "set_update" = engine$set_update,
    "history" = engine$history
  ))
}

#' @export
filter_core_ui <- function(id) {
  ns <- NS(id)
  elements <- list(
    div(
      id = ns("div"),
      uiOutput(outputId = ns("ui"))
    )
  )
  return(elements)
}

filter_core_single <- function(input,
                               output,
                               session,
                               col,
                               get_data,
                               selectInput = purrr::partial(
                                 shiny::selectInput,
                                 label = col,
                                 multiple = TRUE
                               )) {
  ns <- session[["ns"]]
  output[["ui"]] <- renderUI({
    tbl <- get_data()
    req_rows(tbl)
    elements <- list(
      selectInput(
        inputId = ns("selector"),
        choices = dplyr::pull(tbl, dplyr::all_of(col)),
        selected = isolate(input[["selector"]])
      )
    )
    return(elements)
  })
  filter_data <- reactive({
    tbl <- get_data()
    selected <- input[["selector"]]
    if (isTruthy(selected)) {
      tbl <- dplyr::filter(tbl, !!rlang::sym(col) %in% selected)
    }
    return(tbl)
  })
  return(filter_data)
}

#' @export
filter_core <- function(input,
                        output,
                        session,
                        cols,
                        labels = paste("Filter:", cols),
                        get_data = shiny::reactive,
                        selectInput = purrr::partial(
                          shiny::selectInput,
                          multiple = TRUE
                        )) {
  ns <- session[["ns"]]
  output[["ui"]] <- renderUI({
    tbl <- get_data()
    req_rows(tbl)
    ids <- ns(names(tbl))
    elements <- lapply(ids, filter_core_ui)
    return(elements)
  })
  labels <- make_labels_from_cols(cols, labels = labels)
  filters <- list()
  for (i in seq_along(cols)) {
    local({ # force local evaluation to prevent lazy errors from the for loop
      col <- cols[[i]]
      label <- labels[[i]]
      if (length(filters) == 0) {
        filters[[col]] <<- callModule( # Max: Ugh... <<-
          filter_core_single,
          id = col,
          col = col,
          get_data = get_data,
          selectInput = purrr::partial(
            selectInput,
            label = label
          )
        )
      } else {
        prev_data <- filters[[cols[[(i - 1)]]]] # the filtered data at this point
        filters[[col]] <<- callModule( # Max: Ugh... <<-
          filter_core_single,
          id = col,
          col = col,
          get_data = prev_data, # pass the filtered data along the chain
          selectInput = purrr::partial(
            selectInput,
            label = label
          )
        )
      }
    })
  }
  get_steps <- reactive(lapply(filters, function(filter) filter()))
  return(list(
    "get_data" = dplyr::last(filters),
    "get_steps" = get_steps
  ))
}
