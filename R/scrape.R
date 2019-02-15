
#' read as xml
#'
#' @param x input
#' @return xml_document
#' @export
read_as_xml <- function(x) {
  ## if connection
  if (inherits(x, "connection") && inherits(x, "file")) {
    txt <- readLines(x, warn = FALSE)
    close(x)
    x <- txt
    rm(txt)
  } else if (inherits(x, "connection")) {
    x <- tryCatch(xml2::read_html(x),
      error = function(e) return(NULL))
  }
  ## read lines if file
  if (is.character(x) && length(x) == 1 && file.exists(x)) {
    if (grepl("\\.webarchive$|\\.html?$", x)) {
      x <- tryCatch(xml2::read_html(x),
        error = function(e) return(NULL))
        ## if null, report error on read_html
      if (is.null(x)) {
        warning("`read_html()` could not convert `x` into an xml_document.")
        return(NULL)
      }
    } else {
      x <- readr::read_lines(x, progress = FALSE)
    }
  }

  ## if null/empty return null
  if (length(x) == 0) return(NULL)

  ## convert multiple lines into single string
  if (is.character(x) && length(x) > 1) {
    x <- paste(x, collapse = "\n")
  }

  ## read if URL and convert to xml_document
  if (is.character(x) && grepl("^http", x)) {
    x <- tryCatch(xml2::read_html(url(x)),
      error = function(e) return(NULL))
  } else if (is.character(x)) {
    x <- tryCatch(xml2::read_html(x),
      error = function(e) return(NULL))
  }

  ## if null, report error on read_html
  if (is.null(x)) {
    warning("`read_html()` could not convert `x` into an xml_document.")
    return(NULL)
  }

  ## validate data
  if (!inherits(x, "xml_document")) {
    warning("Failed to convert `x` into xml_document")
    return(NULL)
  }

  ## return xml_document
  x
}




any_recursive <- function(x) any(vap_lgl(x, is.recursive))

recursives <- function(x) which(vap_lgl(x, is.recursive))

is_named <- function(x) length(x) > 0 && !is.null()

