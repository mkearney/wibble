#' Wobble
#'
#' Set observation (row)-level for wibble
#'
#' @param .x A wbl
#' @param .n Node on which to set observation level
#' @return A wbl_df
#' @export
wobble <- function(.x, .n) {
  UseMethod("wobble")
}

#' @export
wobble.wbl_df <- function(.x, .n) {
  doc <- attr(.x, "wbl_doc")
  doc <- rvest::html_nodes(doc, .n)
  atts <- rvest::html_attrs(doc)
  doc <- Map(c, doc, atts)
  wibble(doc)
}

#' @export
wobble.xml_document <- function(.x, .n = NULL) {
  if (is.null(.n)) {
    .n <- "body"
  }
  .x <- rvest::html_nodes(.x, .n)
  atts <- lapply(.x, rvest::html_attrs)
  .x <- Map(function(a, b) c(node = list(a), b), .x, atts)
  .x <- lapply(.x, tibble::as_tibble)
  .x <- tbltools::bind_rows_data(.x, fill = TRUE)
  names(.x)[1] <- .n
  names(.x) <- gsub("-", "_", names(.x))
  .x <- wibble(.x)
  if (nrow(.x) > 0 &&"row_names" %in% names(.x)) {
    .x <- .x[names(.x) != "row_names"]
  }
  .x
}

#' @export
wobble.xml_nodeset <- function(.x, .n) {
  wibble(.x)
}
