
webpage_links <- function(internal = NULL, external = NULL) {
  list(internal = internal, external = external)
}

webpage_meta <- function(timestamp = NULL, status = NULL) {
  list(timestamp = timestamp, status = status)
}

#' Webpage object
#'
#' An R6 class to represent source code/data associated with a URL (website).
#'
#' @section Functions:
#' Some text here
#'
#' \describe{
#' \item{one}{description}
#' }
#'
#' @export
Webpage <- R6::R6Class("Webpage", list(
  url = NULL,
  timestamp = NULL,
  data = NULL,
  links = NULL,
  text = NULL,
  tables = NULL,
  initialize = function(url) {
    self$url <- url
    self$data <- read_webdata(url)
    self$timestamp <- Sys.time()
    self$links <- scrape_links(self$data)
    self$text <- scrape_text(self$data)
    self$tables <- scrape_tables(self$data)
  },
  format = function(...) {
    # The first `paste0()` is not necessary but it lines up
    # with the subsequent lines making it easier to see how
    # it will print
    c(
      paste0("# webdata:"),
      paste0("  URL: ", self$url),
      paste0("  Timestamp:  ", self$timestamp)
    )
  }
))
