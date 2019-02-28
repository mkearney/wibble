
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
    x <- read_file_call(path)
  } else {
    x <- webble_call(path)
  }
  as_xml(x)
}
