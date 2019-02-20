subtrunc <- function(x, n) {
  if (nchar(x) > n) {
    x <- paste0(substr(x, 1, n), "\U2026")
  }
  x
}
