
#' Wabble
#'
#' API/URL builder
#'
#' @param url Base URL
#' @param query Named list of query parameters
#' @param path Endpoint
#' @return A wbl_url
#' @examples
#'
#' ## query params as list
#' params <- list(sort = 'desc', page = "4")
#'
#' ## build URL
#' wabble("https://mikewk.com", params, path = "search.json")
#'
#' @export
wabble <- function(url, query = NULL, path = "") {
  UseMethod("wabble")
}

#' @export
wabble.default <- function(url, query = NULL, path = "") {
  wabble_call(url, query, path)
}
