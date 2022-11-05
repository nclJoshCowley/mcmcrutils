#' @param draws object of class `draws`.
#'
#' @family draws methods
#'
#' @section Draws:
#'   Typically output from [draws()], extends [tibble][tibble::tibble()] class
#'   and is assumed to have columns:
#'
#'   - `.chain`, `.iteration`, `.draw` (MCMC sample indices),
#'   - `.term` (parameter information),
#'   - `.value` (MCMC sample value).
