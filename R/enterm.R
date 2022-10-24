#' Convert Vector to Data Frame with Term(s)
#'
#' @param x object. Typically a scalar, vector or matrix.
#' @param name character. Name to be assumed in the `.term` column.
#' @param .value character. Name of the column containing the values.
#'
#' @export
enterm <- function(x, name = "x", .value = ".value") {
  dx <- dim(x) %||% length(x)

  x_dims <- which(array(TRUE, dim = dx), arr.ind = TRUE)
  x_pdims_str <- do.call(paste, c(asplit(x_dims, 2), list(sep = ",")))
  x_term <- term::term(sprintf("%s[%s]", as.character(name), x_pdims_str))

  out <- tibble::tibble(.term = x_term, .value = c(x))
  names(out)[2] <- .value
  return(out)
}
