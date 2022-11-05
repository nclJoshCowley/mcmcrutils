#' @param dv Drawsvis-type tibble. See **Drawsvis** section.
#'
#' @family drawsvis methods
#'
#' @section Drawsvis:
#'   Nested version of [`draws`] that also extends [tibble][tibble::tibble()].
#'   Has one plot per row instead of one draw per row, columns are typically:
#'
#'   - `.chain` (MCMC chain index),
#'   - `.term` (parameter information),
#'   - `.plot` (list column of MCMC visualisations).
