as_xml <- function(x) {
  suppressWarnings(
    xml2:::read_xml.raw(charToRaw(x), "UTF-8", as_html = TRUE,
      options = 289L)
  )
}
