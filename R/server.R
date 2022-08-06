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
    history <- history[order(names(history))]
    keys <- setdiff(purrr::reduce(lapply(history, names), intersect), keys_ignore)
    tbl <- purrr::reduce(history, unique_tbl, by = keys)
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
      keys <- setdiff(intersect(names(update), names(data)), keys_ignore)
      update <- dplyr::anti_join(update, data, by = keys)
      if (isTRUE(as.logical(nrow(update)))) {
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
    history <- history[order(names(history))]
    schema <- purrr::reduce(history, cat_lists)
    return(schema)
  })
  apply_schema <- shiny::reactive({
    schema <- get_schema()
    keys_tbl <- purrr::map2_df(get_data()[names(schema)], schema, factor)
    values_tbl <- get_data()[keys_ignore]
    data <- dplyr::bind_cols(keys_tbl, values_tbl)
    return(data)
  })
  shiny::observeEvent(
    get_data(),
    {
      data <- get_data()
      keys <- setdiff(names(data), keys_ignore)
      schema <- get_schema()
      schema_tbl <- lapply(data[keys], unique)
      if (is.null(names(schema))) {
        update <- schema_tbl
      } else {
        update <- purrr::map2(schema_tbl, schema, setdiff)
      }
      if (any(sapply(update, has_length))) {
        idx <- length(shiny::reactiveValuesToList(history))
        history[[get_timestamp(i = idx)]] <- update
      }
    }
  )
  return(list(
    "get_data" = apply_schema,
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

filter_core_single <- function(
  input,
  output,
  session,
  col,
  get_data = shiny::reactive
) {
  ns <- session[["ns"]]
  output[[col]] <- renderUI({
    tbl <- get_data()
    req_rows(tbl)
    tbl <- dplyr::select(tbl, dplyr::all_of(col))
    ids <- shiny::ns(names(tbl))
    choices <- lapply(tbl, unique)
    elements <- purrr::map2(ids, choices, function(id, choices) {
      shiny::selectInput(
        inputId = id,
        label = id,
        choices = choices,
        selected = isolate(input[[id]])
      )
    })
    return(elements)
  })
}

#' @export
filter_core <- function(
  input,
  output,
  session,
  cols,
  get_data = shiny::reactive
) {
  ns <- session[["ns"]]
  # output[["ui"]] <- renderUI({
  #   tbl <- get_data()
  #   req_rows(tbl)
  #   ids <- ns(names(tbl))
  #   choices <- lapply(tbl, unique)
  #   elements <- purrr::map2(ids, choices, function(id, choices) {
  #     shiny::selectInput(
  #       inputId = id,
  #       label = id,
  #       choices = choices,
  #       selected = isolate(input[[id]])
  #     )
  #   })
  #   return(elements)
  # })
  output[["filter_cols"]] <- renderUI({
    tbl <- get_data()
    req_rows(tbl)
    ids <- ns(names(tbl))
    elements <- lapply(ids, function(id) {
      element <- uiOutput(outputId = id)
      return(element)
    })
    return(elements)
  })

  filters <- lapply(cols, function(col) {
    filter <- callModule(
      filter_core_single,
      id = ns(col),
      col = col,
      get_data = get_data
    )
    return(filter)
  })

  filter_data <- reactive({
    tbl <- get_data()
    req_rows(tbl)
    ids <- colnames(tbl)
    vals <- setNames(
      lapply(ids, function(id, l = input) {
        val <- l[[id]]
        if (shiny::isTruthy(val)) {
          item <- tibble::tibble(!!rlang::sym(id) := setNames(val, id))
          return(item)
        }
      }),
      ids
    )
    out <- purrr::reduce(
      .x = vals,
      .f = function(init, flt) {
        id <- names(flt)
        if (shiny::isTruthy(id)) {
          tbl <- dplyr::filter(init, !!dplyr::sym(id) %in% dplyr::pull(flt, id))
          if (magrittr::not(has_rows(tbl))) {
            tbl <- init
          }
        } else {
          tbl <- init
        }
        return(tbl)
      },
      .init = tbl
    )
    return(out)
  })
  return(list(
    "get_data" = filter_data
  ))
}
