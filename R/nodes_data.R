
as_num <- function(.x) {
  .x <- tfse::regmatches_first(.x, "(-?[0-9][0-9\\.\\,]+)|(-?[\\.][0-9][0-9\\.\\,]+)")
  .x <- gsub("[^0-9\\.-]", "", .x)
  suppressWarnings(as.numeric(.x))
}

as_int <- function(.x) {
  .x <- tfse::regmatches_first(.x,
    "(-?[0-9][0-9\\.\\,]+)|(-?[\\.][0-9][0-9\\.\\,]+)|(-?[0-9]+)")
  if (length(.x) == 0) return(NA_integer_)
  .x <- gsub("[^0-9\\.-]", "", .x)
  suppressWarnings(as.integer(.x))
}


set_obs_level <- function(.x, .n) UseMethod("set_obs_level")

as_nodes_og <- function(.x, .n) {
  if (!inherits(.x, "xml_node")) {
    .x <- get_xml_data(.x)
  }
  if (!grepl("\\[\\d+\\]", .n)) {
    return(rvest::html_nodes(.x, .n))
  }
  i <- tfse::regmatches_first(.n, "\\[\\d+\\]")
  i <- as.integer(gsub("^\\[|\\]", "", i))
  .n1 <- sub("\\[\\d+\\].*", "", .n)
  xml <- rvest::html_nodes(.x, .n1)[[i]]
  if (grepl("\\[\\d+\\].{1,}$", .n)) {
    .n2 <- tfse::regmatches_first(.n, "\\[\\d+\\].*")
    .n2 <- sub("\\[\\d+\\]", "", .n2)
    xml <- rvest::html_nodes(xml, .n2)
  }
  xml
}


as_nodes <- function(.x, .n) {
  if (!inherits(.x, "xml_node")) {
    .x <- get_xml_data(.x)
  }
  if (!grepl("\\[\\d+\\]", .n)) {
    return(dapr::lap(.x, rvest::html_nodes, .n))
  }
  i <- tfse::regmatches_first(.n, "\\[\\d+\\]")
  i <- as.integer(gsub("^\\[|\\]", "", i))
  .n1 <- sub("\\[\\d+\\].*", "", .n)
  .x <- dapr::lap(.x, ~ rvest::html_nodes(.x, .n1)[[i]])
  if (grepl("\\[\\d+\\].{1,}$", .n)) {
    .n2 <- tfse::regmatches_first(.n, "\\[\\d+\\].*")
    .n2 <- sub("\\[\\d+\\]", "", .n2)
    .x <- dapr::lap(.x, rvest::html_nodes, .n2)
  }
  .x
}


as_node <- function(.x, .n) {
  if (!inherits(.x, "xml_node")) {
    .x <- get_xml_data(.x)
  }
  if (!grepl("\\[\\d+\\]", .n)) {
    return(rvest::html_node(.x, .n))
  }
  i <- tfse::regmatches_first(.n, "\\[\\d+\\]")
  i <- as.integer(gsub("^\\[|\\]", "", i))
  .n1 <- sub("\\[\\d+\\].*", "", .n)
  xml <- rvest::html_nodes(.x, .n1)[[i]]
  if (grepl("\\[\\d+\\].{1,}$", .n)) {
    .n2 <- tfse::regmatches_first(.n, "\\[\\d+\\].*")
    .n2 <- sub("\\[\\d+\\]", "", .n2)
    xml <- rvest::html_node(xml, .n2)
  }
  xml
}

set_obs_level.default <- function(.x, .n) {
  xml <- as_nodes_og(.x, .n)
  .x <- dapr::lap(xml, ~ as.list(rvest::html_attrs(.x)))
  for (i in seq_along(.x)) {
    .x[[i]]$.node_id <- i
    .x[[i]] <- .x[[i]][c(".node_id", names(.x[[i]])[names(.x[[i]]) != ".node_id"])]
    .x[[i]] <- tbltools::as_tbl_data(.x[[i]])
  }
  .x <- tbltools::bind_rows_data(.x, fill = TRUE)
  attr(.x, "xml_data") <- xml
  class(.x) <- c("wbl_df", "wbl", "data.frame")
  .x
}

as_tbl_df <- function(x) tibble::as_tibble(get_nodes_data(x))

#' @export
print.wbl_df <- function(x, ...) {
  requireNamespace("tibble", quietly = TRUE)
  tibble:::print.tbl(x, ...)
}

get_nodes_data <- function(.x) {
  #.x <- attr(.x, "nodes_data")
  if (is.null(.x)) {
    return(list())
  }
  .x
}

get_xml_data <- function(.x) {
  .x <- attr(.x, "xml_data")
  if (is.null(.x)) {
    return(list())
  }
  .x
}


add_nodes <- function(.x, ...) UseMethod("add_nodes")

add_nodes.default <- function(.x, ...) {
  xml <- get_xml_data(.x)
  .n <- tbltools:::pretty_dots(...)
  d[[names(.n)]] <- I(dapr::lap(xml, rvest::html_nodes, .n[[1]]))
  attr(.x, "xml_data") <- xml
  .x
}

add_node_chr <- function(.x, ...) UseMethod("add_node_chr")

add_node_chr.default <- function(.x, ...) {
  xml <- get_xml_data(.x)
  .n <- tbltools:::pretty_dots(...)
  v <- rvest::html_node(xml, .n[[1]])
  v <- rvest::html_text(v, trim = TRUE)
  .x[[names(.n)]] <- dapr::vap_chr(v, ~ {
    if (length(.x) == 0) NA_character_ else as.character(.x)}
  )
  attr(.x, "xml_data") <- xml
  class(.x) <- c("wbl_df", "wbl", "data.frame")
  .x
}


add_nodes_chr <- function(.x, ...) UseMethod("add_nodes_chr")

add_nodes_chr.default <- function(.x, ...) {
  xml <- get_xml_data(.x)
  .n <- tbltools:::pretty_dots(...)
  v <- lapply(xml, rvest::html_nodes, .n[[1]])
  v <- dapr::lap(v, ~ {
    rvest::html_text(xml, trim = TRUE) %>% paste(.x, collapse = "\n")}
  )
  .x[[names(.n)]] <- dapr::vap_lgl(v, ~ {
    if (length(.x) == 0) NA_character_ else as.character(.x)}
  )
  attr(.x, "xml_data") <- xml
  class(.x) <- c("wbl_df", "wbl", "data.frame")
  .x
}


add_node_dbl <- function(.x, ...) UseMethod("add_node_dbl")

add_node_dbl.default <- function(.x, ...) {
  xml <- get_xml_data(.x)
  .n <- tbltools:::pretty_dots(...)
  v <- rvest::html_node(xml, .n[[1]])
  v <- rvest::html_text(v, trim = TRUE)
  .x[[names(.n)]] <- dapr::vap_dbl(v, ~ {
    if (length(.x) == 0) NA_real_ else as_num(.x)}
  )
  attr(.x, "xml_data") <- xml
  class(.x) <- c("wbl_df", "wbl", "data.frame")
  .x
}


add_node_int <- function(.x, ...) UseMethod("add_node_int")

add_node_int.default <- function(.x, ...) {
  xml <- get_xml_data(.x)
  .n <- tbltools:::pretty_dots(...)
  v <- rvest::html_node(xml, .n[[1]])
  v <- rvest::html_text(v, trim = TRUE)
  .x[[names(.n)]] <- dapr::vap_int(v, ~ {
    if (length(.x) == 0) NA_integer_ else as_int(.x)}
  )
  attr(.x, "xml_data") <- xml
  class(.x) <- c("wbl_df", "wbl", "data.frame")
  .x
}


add_node_lgl <- function(.x, ...) UseMethod("add_node_lgl")

add_node_lgl.default <- function(.x, ...) {
  xml <- get_xml_data(.x)
  .n <- tbltools:::pretty_dots(...)
  v <- rvest::html_node(xml, .n[[1]])
  v <- rvest::html_text(v, trim = TRUE)
  .x[[names(.n)]] <- dapr::vap_lgl(v, ~ {
    if (length(.x) == 0) NA_character_ else as.logical(.x)}
  )
  attr(.x, "xml_data") <- xml
  class(.x) <- c("wbl_df", "wbl", "data.frame")
  .x
}

add_attr_chr <- function(.x, .n = NULL, ...) UseMethod("add_attr_chr")

add_attr_chr.default <- function(.x, .n = NULL, ...) {
  xml <- get_xml_data(.x)
  .a <- tbltools:::pretty_dots(...)
  if (is.null(.n)) {
    v <- rvest::html_attr(xml, .a[[1]])
  } else {
    v <- rvest::html_node(xml, .n) %>% rvest::html_attr(.a[[1]])
  }
  .x[[names(.a)]] <- dapr::vap_chr(v, ~ {
    if (length(.x) == 0) NA_character_ else as.character(.x)}
  )
  attr(.x, "xml_data") <- xml
  class(.x) <- c("wbl_df", "wbl", "data.frame")
  .x
}


as_nodes_data <- function(.x) {
  tibble::as_tibble(get_nodes_data(.x))
}


