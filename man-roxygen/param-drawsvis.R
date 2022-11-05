#' @param dv Drawsvis-type tibble. See **Drawsvis** section.
#'
#' @family drawsvis methods
#'
#' @section Drawsvis:
#'   Typically output from [drawsvis()], a `drawsvis`-style
#'     [tibble][tibble::tibble()] has one term per row instead of one draws
#'     per row and is therefore assumed to have columns:
#'
#'   - `.chain`. MCMC chain index.
#'   - `.term`. Parameter information.
#'   - `.plot`. List column of MCMC visualisations.
