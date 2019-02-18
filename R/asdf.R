withatts <- function(x) {
  atts <- lapply(x, attributes)
  atts <- atts[lengths(atts) > 0]
  atts <- c(
    attributes(x),
    do.call(base::c, atts)
  )
  atts <- atts[grep("\\.?names$|^$", names(atts), invert = TRUE)]
  if (length(atts) == 0) return(list())
  atts
}

as_df <- function(.x) {
  .x <- dapr::lap(.x, ~ {
    txt <- h_text.xml_document(.x)
    .x <- xml2::as_list(.x)
    .x <- withatts(.x)
    if (length(.x) == 0 && length(txt) == 0) return(list())
    .x$text <- txt
    as.list(.x)
  })
  for (i in seq_along(.x)) {
    if (length(.x[[i]]) == 0) {
      .x[[i]] <- list(.node_id = i)
    } else {
      .x[[i]]$.node_id <- i
    }
  }
  .x <- dapr::lap(.x, tbltools::as_tbl_data)
  vars <- unique(unlist(lapply(.x, names)))
  for (i in seq_along(.x)) {
    if (any(!vars %in% names(.x[[i]]))) {
      navar <- vars[!vars %in% names(.x[[i]])]
      for (j in navar) {
        .x[[i]][[j]] <- NA
      }
    }
  }
  .x <- tbltools::bind_rows_data(.x, fill = TRUE)
  .x[, c(".node_id", names(.x)[names(.x) != ".node_id"])]
}
