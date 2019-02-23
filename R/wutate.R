
#' Character node variable
#'
#' Returns a character vector from the first node of each element of input
#'
#' @param .n A string representing the node from which content should be
#'   extracted.
#' @return A character vector
#' @export
node_chr <- function(.n) UseMethod("node_chr")

#' @export
node_chr.default <- function(.n) {
  .x <- get(".", pos = parent.frame())
  if (length(.n) > 1L) {
    stop("`node_chr()` only expects a single string", call. = FALSE)
  }
  v <- as_node(.x, .n)
  v <- rvest::html_text(v, trim = TRUE)
  dapr::vap_chr(v, ~ {
    if (length(.x) == 0) NA_character_ else as.character(.x)}
  )
}


#' Character nodes variable
#'
#' Returns a character vector from a pasted together-string of all nodes for
#' each element of input
#'
#' @param .n A string representing the nodes from which content should be
#'   extracted for each input element.
#' @return A character vector
#' @export
nodes_chr <- function(.n) UseMethod("nodes_chr")

#' @export
nodes_chr.default <- function(.n) {
  .x <- get(".", pos = parent.frame())
  if (length(.n) > 1L) {
    stop("`node_chr()` only expects a single string", call. = FALSE)
  }
  xml <- as_nodes(.x, .n)
  v <- dapr::lap(xml, rvest::html_nodes, .n)
  v <- dapr::lap(v, rvest::html_text, trim = TRUE)
  dapr::vap_chr(v, ~ {
    if (length(.x) == 0) NA_character_ else paste(as.character(.x), collapse = "\n")}
  )
}

#' Mutate web data
#'
#' Transform and add variables to wibble
#'
#' @param .data A wibble
#' @param ... Expressions resolving as variables in .data
#' @return A wibble
#' @export
wutate <- function(.data, ...) UseMethod("wutate")

#' @importFrom tfse %P%
#' @export
wutate.default <- function(.data, ...) {
  stop("`wutate()` expects an object of class 'wbl' not " %P% class(.data)[1],
    call. = FALSE)
}

#' @export
wutate.wbl <- function(.data, ...) {
  dots <- tbltools:::pretty_dots(...)
  vars <- names(dots)
  for (i in seq_along(dots)) {
    .data[[vars[i]]] <- eval(dots[[i]], list(. = .data), parent.frame())
  }
  .data
}


#' @export
wutate.wbl_df <- function(.data, ...) {
  dots <- tbltools:::pretty_dots(...)
  vars <- names(dots)
  for (i in seq_along(dots)) {
    .data[[vars[i]]] <- eval(dots[[i]], .data, parent.frame())
  }
  .data
}
