# Defaults for NULL values
`%||%` <- function(a, b) if (is.null(a)) b else a

compact <- function(x) {
  x[!vapply(x, is.null, logical(1))]
}

