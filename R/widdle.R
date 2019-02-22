
#' Widdle
#'
#' Read web data
#'
#' @param url Path to file/URL of XML source code
#' @return An xml_document
#' @export
widdle <- function(url) UseMethod("widdle")

#' @export
widdle.character <- function(url) {
  .as_xml(widdler(url))
}
