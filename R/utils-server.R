schema_from_col <- function(col) {
  logger::log_trace("call shiny.table.store::schema_from_col")
  if (is.character(col)) {
    col <- forcats::fct_inorder(factor(col))
  }
  col <- unique(col)
  logger::log_trace("return shiny.table.store::schema_from_col")
  return(col)
}
schema_from_tbl <- function(tbl) {
  logger::log_trace("call shiny.table.store::schema_from_tbl")
  schema <- lapply(tbl, schema_from_col)
  logger::log_trace("return shiny.table.store::schema_from_tbl")
  return(schema)
}
