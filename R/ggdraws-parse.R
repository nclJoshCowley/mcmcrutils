#' Parse ggdraws
#'
#' Parse user input into a chosen (or custom) visualisation function.
#'
#' @param .ggdraws **TODO**.
#' @param ... **TODO**.
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
    "density" = stop("Not implemented yet."),
    "acf" = stop("Not implemented yet."),
    "heatmap" = stop("Not implemented yet.")
  )
}
