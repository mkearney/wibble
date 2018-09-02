
#' scrape URL links from a webpage
#'
#' Scrapes URL links from HTML source code
#'
#' @param rurl URL to desired page. Should include scheme (http/https)
#' @param qs Logical indicating whether to return query strings (part of URL starting with ?)
#' @param pat Pattern from which only matching results should be returned. Defaults to ".",
#'   which matches everything.
#' @return Character vector of URLs.
#' @export
scrape_links <- function(x, qs = TRUE, pat = ".") {
  rurl <- webpage_url(x)
  scheme <- ifelse(grepl("^https", rurl), "https", "http")
  base_url <- paste0(scheme, "://", dirname(gsub("^https?://", "", rurl)))
  if (grepl("https?://.", base_url)) base_url <- paste0(scheme, "://", gsub("^https?://", "", rurl))
  links <- as_html(x) %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href") %>%
    tfse::na_omit() %>%
    ifelse(grepl("^//", .), paste0(scheme, ":", .), .) %>%
    ifelse(grepl("^/", .), paste0(base_url, .), .) %>%
    sub("#.*", "", .)
  if (identical(scheme, "https")) {
    links <- sub("http:", "https:", links)
  }
  if (!qs) {
    links <- sub("\\?.*", "", links)
  }
  links %>%
    unique() %>%
    sort() %>%
    grep(pat, ., value = TRUE)
}


webpage_links <- function(x) UseMethod("page_links")

as_url <- function(x) UseMethod("as_url")

as_url.character <- function(x) {
  structure(
    x,
    class = c("url")
  )
}

print.url <- function(x) {
  cat_call("# URL:")
  x <- paste0("  - ", x)
  cat_call(x, collapse = "\n")
}



webpage_links.character <- function(x) {
  x <- read_webdata(x)
  NextMethod(x)
}


webpage_links.webdata <- function(x) {
  scrape_links(x)
}


webpage_url <- function(x) x$url

scrape_tables <- function(x) {
  as_html(x) %>%
    rvest::html_table(fill = TRUE)
}

scrape_text <- function(x, trim = FALSE) {
  as_html(x) %>%
    rvest::html_nodes("p,h1,h2,h3,h4,h5,h6,h7,li") %>%
    rvest::html_text(trim = trim)
}
