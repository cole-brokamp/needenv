#' Print a needenv configuration
#'
#' Prints the names of resolved environment variables while fully redacting
#' their values. This protects against accidental console output, but it is not
#' a security boundary: individual values remain accessible as ordinary list
#' elements.
#'
#' @param x A `needenv_config` object returned by [needenv()].
#' @param ... Additional arguments, currently unused.
#'
#' @return `x`, invisibly.
#'
#' @export
#' @keywords internal
print.needenv_config <- function(x, ...) {
  cat("<needenv configuration>\n")

  if (length(x) > 0L) {
    cat(paste0(names(x), ": <set>"), sep = "\n")
  }

  invisible(x)
}
