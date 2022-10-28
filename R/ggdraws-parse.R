#' Parse ggdraws
#'
#' Parse user input into a chosen (or custom) visualisation function.
#'
#' @template ggdraws-argument
#'
#' @keywords internal
parse_ggdraws <- function(.ggdraws, ...) {
  UseMethod("parse_ggdraws")
}


#' @rdname parse_ggdraws
#' @keywords internal
parse_ggdraws.default <- function(.ggdraws, ...) {
  return(rlang::as_function(.ggdraws, ...))
}


#' @rdname parse_ggdraws
#' @keywords internal
parse_ggdraws.character <- function(.ggdraws, ...) {
  switch(
    match.arg(.ggdraws, c("trace", "density", "acf", "heatmap")),
    "trace" = ggdraws_trace,
    "density" = ggdraws_dens,
    "acf" = ggdraws_acf,
    "heatmap" = ggdraws_heatmap
  )
}
