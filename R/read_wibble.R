
#' Read wibble
#'
#' Read content as wibble
#'
#' @param x URL, file path, or character vector of source code
#' @details This is a wrapper around xml2's read function.
#' @export
read_wibble <- function(x) {
  UseMethod("read_wibble")
}

#' @export
read_wibble.default <- function(x) {
  x <- suppressWarnings(
    xml2::read_xml(x, encoding = "UTF-8", as_html = TRUE, options = 289L)
  )
  as_wibble(x)
}
