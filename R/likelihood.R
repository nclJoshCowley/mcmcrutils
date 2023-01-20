#' Likelihood Generic (MCMC)
#'
#' Get likelihood contribution of each observation over each MCMC sample.
#'
#' @param object object used to select a method.
#' @template generic-dots
#'
#' @return An [`mcmcarray`][mcmcr::mcmcarray-object] of a `n_obs`-length vector.
#'
#' @export
likelihood <- function(object, ...) {
  UseMethod("likelihood")
}


#' @rdname likelihood
#' @export
log_likelihood <- function(object, ...) {
  UseMethod("log_likelihood")
}


#' @export
log_likelihood.default <- function(object, ...) {
  map_mcmcarray(likelihood(object, ...), log)
}
