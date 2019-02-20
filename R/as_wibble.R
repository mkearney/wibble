#' As wibble
#'
#' Convert object into wibble
#'
#' @param x Input object
#' @return An object inheriting wbl classes
#' @export
as_wibble <- function(x) UseMethod("as_wibble")

#' @importFrom tfse %P%
#' @export
as_wibble.wbl_path <- function(x) {
  ## if URL
  if (grepl("^http", x)) {
    class(x) <- c("wbl_url", "character")
    return(as_wibble(x))
  }
  ## if local file
  if (file.exists(x)) {
    class(x) <- c("wbl_file", "character")
    return(as_wibble(x))
  }

  ## stop with informative message
  stop(sprintf("`as_wibble()` thinks '%s' ", subtrunc(x, 22)) %P%
      "is probably a path, but a corresponding file cannot be found.",
    call. = FALSE)
}

#' @export
as_wibble.wbl_url <- function(x) {
  read_wibble(x)
}

#' @export
as_wibble.wbl_file <- function(x) {
  read_wibble(x)
}

#' @export
as_wibble.connection <- function(x) {
  read_wibble(x)
}

#' @export
as_wibble.character <- function(x) {
  ## if path
  if (is_path(x)) {
    class(x) <- c("wbl_path", "character")
    return(as_wibble(x))
  }
  ## collapse into single x
  if (length(x) > 1) {
    x <- paste(x, collapse = "\n")
  }
  ## read as xml_doc
  read_wibble(x)
}

#' @export
as_wibble.default <- function(x) {
  `class<-`(x, c("wbl", "xml_document", "xml_node"))
}
