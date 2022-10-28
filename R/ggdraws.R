#' Draws Visualisations
#'
#' Various methods to convert a Draws-type tibble into a ggplot2 visualisation.
#'
#' @template draws-method
#' @param mapping Optional `aes`. Updates the mapping in the relevant `geom`.
#' @param ... extra arguments passed to relevant `geom`.
#'
#' @name ggdraws
NULL


#' @rdname ggdraws
#' @export
ggdraws_trace <- function(draws, mapping = NULL, ...) {
  g_aes <- ggplot2::aes(x = .data$.iteration, y = .data$.value)
  if (isFALSE(is.null(mapping))) g_aes <- utils::modifyList(g_aes, mapping)

  ggplot2::ggplot(draws) +
    ggplot2::geom_line(mapping = g_aes, ...) +
    mcmcrutils::scale_x_iterations(name = NULL) +
    ggplot2::labs(x = NULL, y = NULL)
}
