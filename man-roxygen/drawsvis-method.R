#' @param dv tibble. See **Drawsvis** section below.
#'
#' @family drawsvis methods
#'
#' @section Draws:
#'   Typically output from [drawsvis()], a `drawsvis`-style
#'     [tibble][tibble::tibble()] has one term per row instead of one draws
#'     per row and is therefore assumed to have columns:
#'
#'   - `.chain`. MCMC chain index.
#'   - `.term`. Parameter information.
#'   - `.plot`. List column of MCMC visualisations.
