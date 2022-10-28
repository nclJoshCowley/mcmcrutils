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
