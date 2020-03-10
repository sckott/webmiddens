cz <- function(x) Filter(Negate(is.null), x)

assert <- function(x, y) {
  if (!is.null(x)) {
    if (!inherits(x, y)) {
      stop(deparse(substitute(x)), " must be of class ",
           paste0(y, collapse = ", "), call. = FALSE)
    }
  }
  return(x)
}

`%||%` <- function(x, y) {
  if (
    is.null(x) || length(x) == 0 || all(nchar(x) == 0) || all(is.na(x))
  ) y else x
}
