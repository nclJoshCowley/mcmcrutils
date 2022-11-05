#' Drawsvis to Patchwork
#'
#' Convert to a [`patchwork`][patchwork::patchwork-package] object.
#'
#' @template param-drawsvis
#' @param ... extra arguments passed to [`wrap_plots`][patchwork::wrap_plots()].
#' @param separate_chains logical. Input is split across `.chain` when `TRUE`.
#'
#' @return A [`patchwork`][patchwork::patchwork-package] object or a list of
#'   such objects when `separate_chains` is `TRUE`.
#'
#' @export
drawsvis_to_patchwork <- function(dv, ..., separate_chains = TRUE) {
  if (isFALSE(separate_chains)) return(patchwork::wrap_plots(dv$.plot, ...))

  dv_list <- draws_separate_chains(dv)
  return(lapply(lapply(dv_list, `[[`, ".plot"), patchwork::wrap_plots, ...))
}
