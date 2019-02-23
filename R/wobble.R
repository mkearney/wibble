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
  wibble(doc)
}

#' @export
wobble.xml_document <- function(.x, .n) {
  .x <- xml2::html_nodes(.x, .n)
  wibble(.x)
}

#' @export
wobble.xml_nodeset <- function(.x, .n) {
  wibble(.x)
}
