#' Draws, Separate Chains
#'
#' Split draws tibble into a list of `draws` for chain-by-chain manipulation.
#'
#' @template draws-method
#' @param .keep logical. When `TRUE`, the chain column is kept in the output.
#'
#' @export
draws_separate_chains <- function(draws, .keep = FALSE) {
  out <- unname(split(draws, draws$.chain))

  if (.keep) return(out)

  return(lapply(out, function(.x) .x[-match(".chain", names(.x))]))
}
