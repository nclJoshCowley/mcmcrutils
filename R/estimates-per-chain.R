#' Estimates per Chain
#'
#' Calculate the estimates of an MCMC object on a chain-per-chain basis.
#'
#' @param x [`mcmcarray`][mcmcr::mcmcarray-object()] object.
#' @inheritParams mcmcr::estimates.mcmcarray
#'
#' @family mcmcarray methods
#'
#' @export
estimates_per_chain <- function(x, fun, as_df = FALSE, ...) {
  mcmcr::chk_mcmcarray(x)

  mcmcarray_per_chain <-
    lapply(
      asplit(x, MARGIN = 1),
      function(.x) structure(.x, class = "mcmcarray", .Dim = c(1, dim(.x)))
    )

  lapply(mcmcarray_per_chain, mcmcr::estimates, fun = fun, as_df = as_df, ...)
}
