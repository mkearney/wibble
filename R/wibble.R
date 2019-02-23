
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
  wibble_call(.x)
}
