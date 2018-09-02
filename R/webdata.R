
read_webdata <- function(x) {
  structure(
    httr::GET(x),
    class = c("webdata", "response")
  )
}

cat_call <- function(..., collapse = "", fill = TRUE) {
  dots <- list(...)
  cat(paste(unlist(dots), collapse = collapse), fill = fill)
}

is_html <- function(x) {
  any(grepl("\\<\\S+\\/\\>", x))
}

is_json <- function(x) {
  any(grepl("^\\s?(\\[|\\{).*\\b\\S+\"?\\: \"[^\"]+\"((\\,\\s)|([[:punct:]]$))", x))
}


from_json <- function(x) {
  m <- gregexpr("(?<=[[:alpha:]])\\S+(?=[[:alpha:]]:)", x, perl = TRUE)
  if (m[[1]][1] > 0) {
    m <- lapply(m, function(.x) .x - 1)
    m <- lapply(m, function(.x) {
      attr(.x, "match.length") <- attr(.x, "match.length") + 2
      return(.x)
    })
    regmatches(x, m) <- lapply(regmatches(x, m), dQuote)
  }
  jsonlite::fromJSON(x)
}

print.webdata <- function(x) {
  o <- capture.output(print(tfse::set_class(x, "response")))
  o[1] <- paste0("## webdata:\n - URL: ", gsub("Response \\[|\\]$", "", o[1]))
  o[2:5] <- sub("^\\s+", " - ", o[2:5])
  cat_call(o[1:5], collapse = "\n")
  txt <- as_text(x)
  if (is_html(txt)) {
    len <- nchar(txt)
  } else if (is_json(txt)) {
    x <- tryCatch(from_json(txt), error = function(e) NULL)
    len <- length(x)
  } else {
    len <- length(x)
  }
  cat_call(" - Length: ", len)
}

is_urlencoded <- function(x) {
  if (!is.character(x)) return(FALSE)
  any(grepl("\\%\\d", x))
}
as_text <- function(x) UseMethod("as_text")
as_text.webdata <- function(x) {
  class(x) <- "response"
  x <- httr::content(x, as = "text", encoding = "UTF-8")
  if (is_urlencoded(x)) {
    x <- URLdecode(x)
  }
  x
}

as_html <- function(x) UseMethod("as_html")
as_html.webdata <- function(x) {
  class(x) <- "response"
  httr::content(x)
}

as_json <- function(x) UseMethod("as_json")
as_json.webdata <- function(x) {
  x <- as_text(x)
  from_json(x)
}
