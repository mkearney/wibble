
#' Wibble
#'
#' A web data frame
#'
#' @param .x Input object
#' @return A wbl data frame
#' @examples
#'
#' ## return wbl_df
#' wibble(list(a = 1:5, b = TRUE, c = NULL))
#'
#' @export
wibble <- function(.x) {
  UseMethod("wibble")
}

#' @export
wibble.default <- function(.x) {
  wibble_call(.x)
}

#' @export
wibble.xml_document <- function(.x) {
  hb <- c("head", "body")
  n <- lapply(hb, function(.n) xml2::as_list(xml2::xml_find_all(.x, .n)))
  names(n) <- hb
  n <- wibble(n)
  attr(n, "wbl_doc") <- .x
  n
}

#' @export
wibble.xml_nodeset <- function(.x) {
  hb <- unique(unlist(lapply(.x, function(.i) names(xml2::as_list(.i)))))
  hb <- hb[hb != ""]
  n <- lapply(hb, function(.n) xml2::as_list(xml2::xml_find_all(.x, .n)))
  names(n) <- hb
  n <- wibble(n)
  attr(n, "wbl_doc") <- .x
  n
}

#' @export
wibble.xml_node <- function(.x) {
  hb <- unique(names(xml2::as_list(.x)))
  n <- lapply(hb, function(.n) xml2::as_list(xml2::xml_find_all(.x, .n)))
  names(n) <- hb
  n <- wibble(n)
  attr(n, "wbl_doc") <- .x
  n
}

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
