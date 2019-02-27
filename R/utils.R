subtrunc <- function(x, n) {
  if (nchar(x) > n) {
    x <- paste0(substr(x, 1, n), "\U2026")
  }
  x
}


is_list_alist <- function(x) {
  is.list(x) && length(x) == 1 && length(names(x)) == 0 &&
    is.list(x[[1]]) && length(names(x[[1]])) > 0
}

do_call <- function(what, ...) {
  args <- eval(substitute(alist(...)))
  if (is_list_alist(args)) {
    args <- args[[1]]
  }
  if (length(args) == 0) {
    args <- alist()
  }
  if (!is.list(args)) {
    args <- as.list(args)
  }
  do.call(what, args, quote = FALSE, envir = parent.frame())
}


capture_dots <- function (...) {
  eval(substitute(alist(...)), envir = parent.frame())
}
