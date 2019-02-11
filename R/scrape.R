
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

#' grab json
#'
#' @param x input
#' @return if json found then parsed list
#' @examples
#' \dontrun{
#' ## scrape espn
#' e <- parse_json("http:/www.espn.com/")
#' str(e, 3)
#'
#' ## convert html text
#' parse_json('<html>{\"mpg\":21,\"cyl\":6,\"disp\":160,\"lgl\":false}</html>')
#' }
#' @export
parse_json <- function(x) UseMethod("parse_json")

#' @export
parse_json.default <- function(x) {
  x <- as.character(x)
  parse_json(x)
}

#' @export
parse_json.file <- function(x) {
  x <- xml2::read_html(x)
  parse_json(x)
}

#' @export
parse_json.url <- function(x) {
  x <- xml2::read_html(x)
  parse_json(x)
}

#' @export
parse_json.xml_document <- function(x) {
  x <- paste0(as.character(x), collapse = "\n")
  parse_json(x)
}


#' @export
parse_json.character <- function(x) {
  ## if URL
  if (length(x) == 1 && grepl("^http", x)) {
    x <- url(x)
    return(parse_json(x))
  }
  ## if local file
  if (length(x) == 1 && file.exists(x)) {
    x <- file(x)
    return(parse_json(x))
  }
  ## collapse into single x
  if (length(x) > 1) {
    x <- paste(x, collapse = "\n")
  }

  ## try it
  if (grepl("^\\[|^\\{", x) && grepl("\\}$|\\]$", x)) {
    tfse::print_complete("Detected JSON input")
    j <- try_from_json(x)
    if (!is.null(j)) {
      tfse::print_complete("Conversion via `jsonlite::fromJSON()` successful!")
      return(j)
    }
    cat("\U274C Initial conversion via `jsonlite::fromJSON()` failed",
      fill = TRUE)
  }

  ## extract [{content}]
  m <- tfse::gregexpr_(x, "\\[\\{\"[^\\]]+(?=\\}\\])")
  d <- unlist(regmatches(x, m), use.names = FALSE)
  if (length(d) > 0) {
    tfse::print_complete(
      sprintf("'[{...}]' JSON strings detected: %s", num_length(d)))
    d <- paste0(d, "}]")
    regmatches(x, m) <- ""
  }

  ## extract {content}
  m <- tfse::gregexpr_(x, "\\{\"[^\\}]+(?=\\})")
  e <- unlist(regmatches(x, m), use.names = FALSE)
  if (length(e) > 0) {
    tfse::print_complete(
      sprintf(" '{...}'  JSON strings detected: %s", num_length(e)))
    e <- paste0(e, "}")
    regmatches(x, m) <- ""
  }

  ## combine extracted content
  x <- c(d, e)
  ## convert from JSON to list
  x <- dapr::lap(x, try_from_json)
  tfse::print_complete(sprintf(
    "JSON conversions success/total : %s/%s",
    num(sum(lengths(x) > 0)), num_length(x)))
  ## return parsed observations
  x[lengths(x) > 0L]
}

is_integer_like <- function(x) {
  if (!is.numeric(x)) return(FALSE)
  if (is.integer(x)) return(TRUE)
  all((tfse::na_omit(x) %% 1) == 0)
}

num <- function(x) {
  if (is.character(x) && all(grepl("^\\d+$", x))) {
    x <- as.numeric(x)
  }
  if (is_integer_like(x) || (is.numeric(x) & all(x >= 100.0))) {
    x <- formatC(x, digits = 8, big.mark = ",")
  }
  if (is.numeric(x)) {
    sp <- getOption("scipen")
    on.exit(options(scipen = sp))
    options(scipen = 10)
    x <- formatC(x, digits = 4, big.mark = ",")
  }
  tfse::trim_ws(x)
}

num_length <- function(x) {
  num(length(x))
}

tbl_and_bind_one <- function(x) {
  tryCatch(
    x <- suppressWarnings(tbltools::bind_rows_data(
      dapr::lap(x, tbltools::as_tbl_data), fill = TRUE)),
    error = function(e) x)
}

#' Convert to tbl and then bind rows
#'
#' Convert lists to data frames then bind rows
#'
#' @param x Input list
#' @return A list of data frames and lists
#' @export
tbl_and_bind <- function(x) {
  nms <- dapr::vap_chr(x, ~ paste(sort(names(.x)), collapse = ","))
  x <- split(x, as.integer(factor(nms)))
  dapr::lap(x, tbl_and_bind_one)
}

parse_json2 <- function(x) {
  ## validate input
  stopifnot(is.character(x))
  ## when x is chr and len 1
  if (is.character(x) && length(x) == 1L) {
    ## if json
    if (grepl("^\\[\\{", x)) {
      j <- tryCatch(jsonlite::fromJSON(x), error = function(e) return(NULL))
      if (!is.null(j)) {
        j <- tfse::peel_lists(j)
        if (is.data.frame(j)) j <- tryCatch(tbltools::as_tbl(j),
          error = function(e) return(j))
        return(j)
      }
    } else if (file.exists(x) || grepl("^http", x)) {
      ## read as xml
      x <- tryCatch(read_as_xml(x), error = function(e) return(x))
      ## convert to char
      x <- as.character(x)
    }
  }
  ## if null/empty return null
  if (length(x) == 0) return(NULL)

  ## convert multiple lines into single string
  if (is.character(x) && length(x) > 1) {
    x <- paste(x, collapse = "\n")
  }

  ## initialize output vector
  o <- list()

  ## swing for fences
  o[[length(o) + 1L]] <- safely_parse_json(x, "\\[\\{\".*\\}\\](?!\\,)")
  o[[length(o) + 1L]] <- safely_parse_json(x, "(?<!\\[)\\{\".*\\}(?!\\,|\\]|\\})")
  o[[length(o) + 1L]] <- safely_parse_json(x, "\\{\".*\\}(?=;</script>)")
  o[[length(o) + 1L]] <- safely_parse_json(x, "\\{\".*\\}(?=(;|\\s))")
  o[[length(o) + 1L]] <- safely_parse_json(x, "\\{\".*\\}(?!\\,)")
  o[[length(o) + 1L]] <- safely_parse_json(x, "\\{.*\\}")

  ## return o
  o <- o[lengths(o) > 0]
  if (length(o) == 0) return(o)
  o <- lapply(o, tfse::peel_lists)
  o <- unique(o)
  tfse::peel_lists(o)
}


regex_match <- function(m) {
  o <- rep(FALSE, length(m))
  o[lengths(m) > 1] <- TRUE
  o[lengths(m) == 1] <- vapply(m[lengths(m) == 1], function(x) x[1] > -1, FUN.VALUE = logical(1),
    USE.NAMES = FALSE)
  o
}

safely_parse_json <- function(x, pat) {
  m <- tfse::gregexpr_(x, pat)
  if (regex_match(m)) {
    x <- tfse::regmatches_(x, pat, drop = TRUE)
  }
  if (length(x) > 1) {
    x <- lapply(x, safely_fromJSON)
  } else {
    x <- safely_fromJSON(x)
  }
  x[lengths(x) > 0]
}

safely_fromJSON <- function(x) {
  if (length(x) == 0) return(NULL)
  fj <- function(x) {
    o <- tryCatch(jsonlite::fromJSON(x), error = function(e) return(NULL))
    if (is.null(o)) {
      o <- tryCatch(jsonlite::fromJSON(paste0("[", x, "]")), error = function(e) return(NULL))
    }
    o
  }
  x <- purrr::map(x, fj)
  if (is.null(x) || all(lengths(x) == 0)) return(NULL)
  x <- x[lengths(x) > 0]
  names(x) <- paste0("j", seq_along(x))
  x <- flattendfs(x)
  tfse::peel_lists(x)
}


flattendfs <- function(x) {
  if (is.atomic(x[[1]]) && length(x[[1]]) == 1L) return(NULL)
  if (!is.data.frame(x) && is.data.frame(x[[1]])) x <- tbltools::as_tbl(x[[1]])
  if (!is.data.frame(x) && is.list(x[[1]]) &
      length(x[[1]]) == 1 & is.data.frame(x[[1]][[1]])) x <- tbltools::as_tbl(x[[1]][[1]])
  if (is.data.frame(x)) x <- tbltools::as_tbl(x)
  if (is.list(x[[1]]) && length(unique(lengths(x[[1]]))) == 1) {
    x <- tbltools::as_tbl(x[[1]])
  }
  x
}


any_recursive <- function(x) any(vap_lgl(x, is.recursive))
recursives <- function(x) which(vap_lgl(x, is.recursive))

is_named <- function(x) length(x) > 0 && !is.null()


try_from_json <- function(x) {
  tryCatch(jsonlite::fromJSON(x),
    error = function(e) list())
}
