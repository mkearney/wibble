


h_nodes <- function(.x, .n) UseMethod("h_nodes")

h_nodes.character <- function(.x, .n) {
  .x <- xml2::read_html(.x)
  h_nodes(.x, .n)
}

h_nodes.xml_document <- function(.x, .n) {
  xml2::xml_find_all(.x, selectr::css_to_xpath(.n))
}

h_nodes.xml_nodeset <- function(.x, .n) {
  xml2::xml_find_all(.x, selectr::css_to_xpath(.n))
}


h_node <- function(.x, .n) UseMethod("h_node")

h_node.character <- function(.x, .n) {
  .x <- xml2::read_html(.x)
  h_node(.x, .n)
}

h_node.xml_document <- function(.x, .n) {
  xml2::xml_find_first(.x, selectr::css_to_xpath(.n))
}

h_node.xml_nodeset <- function(.x, .n) {
  xml2::xml_find_first(.x, selectr::css_to_xpath(.n))
}


h_text <- function(.x, ...) UseMethod("h_text")

h_text.character <- function(.x, ...) {
  .x <- xml2::read_html(.x)
  h_text(.x, ...)
}

do_call <- function(what, args) {
  do.call(what, args, quote = FALSE, envir = parent.frame())
}

h_text.xml_document <- function(.x, ...) {
  args <- capture_dots(x = .x, ...)
  args <- tfse::add_arg_if(args, trim = TRUE)
  do_call(xml2::xml_text, args)
}


h_text.xml_nodeset <- function(.x, ...) {
  args <- capture_dots(x = .x, ...)
  args <- tfse::add_arg_if(args, trim = TRUE)
  do_call(xml2::xml_text, args)
}

capture_dots <- function (...) {
  eval(substitute(alist(...)), envir = parent.frame())
}

