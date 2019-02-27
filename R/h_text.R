#' h_text
#'
#' Identify and return node object via CSS
#'
#' @param .x Input nodes object
#' @param ... Passed to xml2::xml_text()
#' @return Character vector or list of character vectors
#'
#' @export
h_text <- function(.x, ...) UseMethod("h_text")

#' @export
h_text.character <- function(.x, ...) {
  .x <- xml2::read_html(.x)
  h_text(.x, ...)
}


#' @export
h_text.xml_document <- function(.x, ...) {
  args <- capture_dots(x = .x, ...)
  args <- tfse::add_arg_if(args, trim = TRUE)
  do_call(xml2:::xml_text.xml_nodeset, args)
}


#' @export
h_text.xml_nodeset <- function(.x, ...) {
  args <- capture_dots(x = .x, ...)
  args <- tfse::add_arg_if(args, trim = TRUE)
  dapr::lap(.x, h_text, ...)
}

#' @export
h_text.xml_node <- function(.x, ...) {
  args <- capture_dots(x = .x, ...)
  args <- tfse::add_arg_if(args, trim = TRUE)
  do_call(x_text, args)
}

x_text <- function(x) x$res

#' @export
h_text.list <- function(.x, ...) {
  dapr::vap_chr(.x, h_text, ...)
}


