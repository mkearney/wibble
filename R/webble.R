
#' Webble
#'
#' Read web data
#'
#' @param url Path to file/URL of XML source code
#' @return An xml_document
#' @export
webble <- function(url) UseMethod("webble")

#' @export
webble.character <- function(url) {
  webble_call(url)
}
