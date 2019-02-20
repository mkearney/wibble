
#' As wobble
#'
#' Convert wibbles to wobbles data frame with a set observation level
#'
#' @param .x Input wibble
#' @param .n Node on which to set observation level
#' @return A wobble with attributes and .node_id as initial variables
#' @export
as_wobble <- function(.x, .n) {
  UseMethod("as_wobble")
}

#' @export
as_wobble.wbl <- function(.x, .n) {
  set_obs_level(.x, .n)
}
