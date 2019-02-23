
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
  requireNamespace("tibble", quietly = TRUE)
  wibble_call(.x)
}

#' @export
wibble.wbl_df <- function(.x) {
  .x
}


#' @export
wibble.xml_document <- function(.x) {
  nn <- c("head", "body")
  nd <- lapply(nn, function(.n) list(xml2::xml_find_all(.x, .n)))
  names(nd) <- nn
  nd <- wibble(nd)
  attr(nd, "wbl_doc") <- .x
  nd
}

#' @export
wibble.xml_nodeset <- function(.x) {
  nn <- unique(unlist(lapply(.x, function(.i) names(xml2::as_list(.i))),
    use.names = FALSE))
  nn <- nn[nn != ""]
  nd <- lapply(nn, function(.n) xml2::as_list(xml2::xml_find_all(.x, .n)))
  names(nd) <- nn
  nd <- wibble(nd)
  attr(nd, "wbl_doc") <- .x
  nd
}

#' @export
wibble.xml_node <- function(.x) {
  nn <- unique(names(xml2::as_list(.x)))
  nn <- nn[nn != ""]
  nd <- lapply(nn, function(.n) xml2::as_list(xml2::xml_find_all(.x, .n)))
  names(nd) <- nn
  nd <- wibble(nd)
  attr(nd, "wbl_doc") <- .x
  nd
}

