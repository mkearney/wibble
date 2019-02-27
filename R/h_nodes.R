#' h_node
#'
#' Identify and return node object via CSS
#'
#' @param .x Input nodes object
#' @param .n CSS used to identify the node(s)
#' @return A node object
#'
#' @export
h_node <- function(.x, .n) UseMethod("h_node")

#' @export
h_node.character <- function(.x, .n) {
  .x <- xml2::read_html(.x)
  h_node(.x, .n)
}

#' @export
h_node.xml_document <- function(.x, .n) {
  xml2::xml_find_first(.x, selectr::css_to_xpath(.n))
}

#' @export
h_node.xml_nodeset <- function(.x, .n) {
  xml2::xml_find_first(.x, selectr::css_to_xpath(.n))
}

#' @export
h_node.xml_node <- function(.x, .n) {
  xml2::xml_find_first(.x, selectr::css_to_xpath(.n))
}

#' @export
h_node.list <- function(.x, .n) {
  dapr::lap(.x, h_node, .n)
}

#' h_nodes
#'
#' Identify and return node objects via CSS
#'
#' @param .x Input nodes object
#' @param .n CSS used to identify the node(s)
#' @return Nodes or list of nodes object
#'
#' @export
h_nodes <- function(.x, .n) UseMethod("h_nodes")

#' @export
h_nodes.character <- function(.x, .n) {
  .x <- xml2::read_html(.x)
  h_nodes(.x, .n)
}

#' @export
h_nodes.xml_document <- function(.x, .n) {
  xml2::xml_find_all(.x, selectr::css_to_xpath(.n))
}

#' @export
h_nodes.xml_nodeset <- function(.x, .n) {
  dapr::lap(.x, h_nodes, .n)
}

#' @export
h_nodes.xml_node <- function(.x, .n) {
  xml2::xml_find_all(.x, selectr::css_to_xpath(.n))
}

#' @export
h_nodes.list <- function(.x, .n) {
  dapr::lap(.x, h_nodes, .n)
}
