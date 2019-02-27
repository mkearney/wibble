
#' Webble
#'
#' Read web data
#'
#' @param path Path/URL to page of XML source code
#' @return An xml_document
#' @export
webble <- function(path) UseMethod("webble")

#' @export
webble.default <- function(path) {
  stopifnot(
    length(path) == 1,
    is.character(path)
  )
  if (!grepl("http", path) && file.exists(path)) {
    x <- paste(tfse::readlines(path), collapse = "\n")
    suppressWarnings(xml2:::read_xml.raw(charToRaw(x),
      "UTF-8", as_html = TRUE, options = 289L))
  } else {
    webble_call(path)
  }
}
