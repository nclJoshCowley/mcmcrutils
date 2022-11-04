#' Example Draws Object(s)
#'
#' Draws-type tibbles for the [`mcmcr_example`][mcmcr::mcmcr_example].
#'
#' @format List of three tibbles with columns
#' - `.chain`, `.iteration`, `.draw`. MCMC sample indices.
#' - `.term`. Parameter information.
#' - `.value`. MCMC sample value.
"draws_example"


#' Example Drawsvis Object(s)
#'
#' Drawsvis-type tibbles for the [`mcmcr_example`][mcmcr::mcmcr_example].
#'
#' @format List of three tibbles with columns
#' - `.chain`. MCMC chain index.
#' - `.term`. Parameter information.
#' - `.plot`. MCMC visualisation.
"drawsvis_example"


#' Example JAGS Algorithm Controls
#'
#' Various controls to be used in JAGS sampling
#'
#' @format A `list` with elements: `n.adapt`, `n.update`, `n.iter`, `n.thin`
#'   and `n.chains`.
#'
#' @name jags-n-data
NULL

#' @describeIn jags-n-data Used for code testing and verification.
"jags_n_debug"

#' @describeIn jags-n-data Used for analysis stage on full data, fewer samples.
"jags_n_short"

#' @describeIn jags-n-data Used for analysis stage **with** thinning.
"jags_n_thin"
