withatts <- function(x) {
  atts <- attributes(x)
  if (length(atts) == 0) return(list())
  atts <- atts[!names(atts) %in% c("names", "class")]
  atts
}

as_df <- function(.x) {
  .x <- dapr::lap(.x, ~ {
    txt <- h_text.xml_document(.x)
    .x <- xml2::as_list(.x)
    .x <- withatts(.x)
    if (length(.x) == 0) return(data.frame())
    .x$text <- txt
    .x
  } )
  for (i in seq_along(.x)) {
    if (length(.x[[i]]) == 0) {
      .x[[i]] <- list(node = i)
    } else {
      .x[[i]]$node <- i
    }
  }
  .x <- .x[lengths(.x) > 0]
  .x <- dapr::lap(.x, tbltools::as_tbl_data)
  .x <- tbltools::bind_rows_data(.x, fill = TRUE)
  .x[, c("node", names(.x)[names(.x) != "node"])]
}
