#' Unpack recursive list
#'
#' Flattens recursive lists, concatenating by name
#'
#' @param x Input list
#' @export
unpack <- function(x) UseMethod("unpack")

#' @export
unpack.default <- function(x) {
  if (!is.recursive(x)) return(x)
  if (any(!are_unpackable(x))) {
    e <- x[!are_unpackable(x)]
    x[!are_unpackable(x)] <- NULL
  } else {
    e <- list()
  }
  for (i in seq_along(x)) {
    if (is_unpackable(x[[i]])) {
      nms <- names(x[[i]])
      for (j in nms) {
        e[[j]] <- c(e[[j]], x[[i]][[j]])
      }
    }
  }
  e
}

#' Coerce list into list of data frames
#'
#' Combines same-length elements of input list into data frames
#'
#' @param x Input list
#' @export
coerce_tbls <- function(x) {
  l <- lengths(x)
  uql <- unique(l)
  d <- vector("list", length(uql))
  for (i in seq_along(uql)) {
    d[[i]] <- tbltools::as_tbl_data(x[l == uql[i]])
  }
  d
}

#' Unpack all
#'
#' Smashes lists until no longer unpackable
#'
#' @param x Input list
#' @return Complete unpacked
#' @export
unpack_all <- function(x) {
  UseMethod("unpack_all")
}

#' @export
unpack_all.default <- function(x) {
  while (any_unpackable(x)) {
    .x <- unpack.default(x)
    if (identical(x, .x)) break
    x <- .x
  }
  x[dapr::vap_lgl(x, is.list)] <- dapr::lap(
    x[dapr::vap_lgl(x, is.list)], unlist, use.names = FALSE)
  x
}


is_unpackable <- function(x) {
  if (length(x) == 0) return(FALSE)
  is.recursive(x) &&
    (!is.null(names(x)) && !all(names(x) == ""))
}

are_unpackable <- function(x) {
  dapr::vap_lgl(x, is_unpackable)
}

any_unpackable <- function(x) {
  any(are_unpackable(x))
}
