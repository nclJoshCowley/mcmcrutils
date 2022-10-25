#' Draws, Unnest Term
#'
#' Splits the `.term` column of a draws object into a data frame column.
#'
#' @template draws-method
#' @param .names character. Dimension colnames, defaults to `dim_*`.
#'
#' @export
draws_unnest_term <- function(draws, .names) {
  t_indices <- as.data.frame(do.call(rbind, term::tindex(draws$.term)))
  t_nms <- term::pars_terms(draws$.term)
  nd <- NCOL(t_indices)

  if (missing(.names)) .names <- sprintf("dim_%s", seq_len(nd))

  if (length(.names) != nd) {
    stop("Dimension mismatch (indices: ", nd, ", names: ", length(.names), ")")
  }

  draws$.term <- tibble::tibble(nm = t_nms, stats::setNames(t_indices, .names))

  return(draws)
}
