
as_num <- function(.x) {
  .x <- tfse::regmatches_first(.x, "(-?[0-9][0-9\\.\\,]+)|(-?[\\.][0-9][0-9\\.\\,]+)")
  .x <- gsub("[^0-9\\.-]", "", .x)
  suppressWarnings(as.numeric(.x))
}

as_int <- function(.x) {
  .x <- tfse::regmatches_first(.x, "(-?[0-9][0-9\\.\\,]+)|(-?[\\.][0-9][0-9\\.\\,]+)")
  .x <- gsub("[^0-9\\.-]", "", .x)
  suppressWarnings(as.integer(.x))
}


set_obs_level <- function(.x, .n) UseMethod("set_obs_level")

set_obs_level.default <- function(.x, .n) {
  .x <- rvest::html_nodes(.x, .n)
  tfse::add_class(.x, "xml_nodes_data")
}


get_nodes_data <- function(.x) {
  .x <- attr(.x, "nodes_data")
  if (is.null(.x)) {
    .x <- list()
  }
  .x
}

add_nodes <- function(.x, ...) UseMethod("add_nodes")

add_nodes.default <- function(.x, ...) {
  d <- get_nodes_data(.x)
  .n <- tbltools:::pretty_dots(...)
  d[[names(.n)]] <- I(dapr::lap(.x, rvest::html_nodes, .n[[1]]))
  attr(.x, "nodes_data") <- d
  .x
}

add_node_chr <- function(.x, ...) UseMethod("add_node_chr")

add_node_chr.default <- function(.x, ...) {
  d <- get_nodes_data(.x)
  .n <- tbltools:::pretty_dots(...)
  v <- rvest::html_node(.x, .n[[1]])
  v <- rvest::html_text(v, trim = TRUE)
  d[[names(.n)]] <- dapr::vap_chr(v,
    ~ if (length(.x) == 0) NA_character_ else as.character(.x))
  attr(.x, "nodes_data") <- d
  .x
}


add_nodes_chr <- function(.x, ...) UseMethod("add_node_chr")

add_nodes_chr.default <- function(.x, ...) {
  d <- get_nodes_data(.x)
  .n <- tbltools:::pretty_dots(...)
  v <- lapply(.x, rvest::html_nodes, .n[[1]])
  v <- dapr::lap(v,
    ~ rvest::html_text(.x, trim = TRUE) %>% paste(.x, collapse = "\n"))
  d[[names(.n)]] <- dapr::vap_chr(v,
    ~ if (length(.x) == 0) NA_character_ else as.character(.x))
  attr(.x, "nodes_data") <- d
  .x
}


add_node_dbl <- function(.x, ...) UseMethod("add_node_dbl")

add_node_dbl.default <- function(.x, ...) {
  d <- get_nodes_data(.x)
  .n <- tbltools:::pretty_dots(...)
  v <- rvest::html_node(.x, .n[[1]])
  v <- rvest::html_text(v, trim = TRUE)
  d[[names(.n)]] <- dapr::vap_dbl(v,
    ~ if (length(.x) == 0) NA_character_ else as_num(.x))
  attr(.x, "nodes_data") <- d
  .x
}


add_node_int <- function(.x, ...) UseMethod("add_node_int")

add_node_int.default <- function(.x, ...) {
  d <- get_nodes_data(.x)
  .n <- tbltools:::pretty_dots(...)
  v <- rvest::html_node(.x, .n[[1]])
  v <- rvest::html_text(v, trim = TRUE)
  d[[names(.n)]] <- dapr::vap_int(v,
    ~ if (length(.x) == 0) NA_character_ else as_int(.x))
  attr(.x, "nodes_data") <- d
  .x
}


add_node_lgl <- function(.x, ...) UseMethod("add_node_lgl")

add_node_lgl.default <- function(.x, ...) {
  d <- get_nodes_data(.x)
  .n <- tbltools:::pretty_dots(...)
  v <- rvest::html_node(.x, .n[[1]])
  v <- rvest::html_text(v, trim = TRUE)
  d[[names(.n)]] <- dapr::vap_lgl(v,
    ~ if (length(.x) == 0) NA_character_ else as.logical(.x))
  attr(.x, "nodes_data") <- d
  .x
}

add_attr_chr <- function(.x, .n = NULL, ...) UseMethod("add_attr_chr")

add_attr_chr.default <- function(.x, .n = NULL, ...) {
  d <- get_nodes_data(.x)
  .a <- tbltools:::pretty_dots(...)
  if (is.null(.n)) {
    v <- rvest::html_attr(.x, .a[[1]])
  } else {
    v <- rvest::html_node(.x, .n) %>% rvest::html_attr(.a[[1]])
  }
  d[[names(.a)]] <- dapr::vap_chr(v,
    ~ if (length(.x) == 0) NA_character_ else as.character(.x))
  attr(.x, "nodes_data") <- d
  .x
}


as_nodes_data <- function(.x) {
  tibble::as_tibble(get_nodes_data(.x))
}


