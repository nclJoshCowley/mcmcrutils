#' @param .ggdraws see **ggdraws** section
#' @param ... extra arguments passed to invoked `.ggdraws` function.
#'
#' @section ggdraws:
#'   The role of the `.ggdraws` argument is invoke a function capable of
#'   converting a (typically filtered) Draws-type tibble into a `ggplot2`
#'   object.
#'
#'   For advanced users it can be a custom function (or formula) that should
#'   take the signature `.f(draws, ...)`.
#'
#'   Alternatively, one can supply a character argument to invoke a built-in
#'   method.
#'
#'   - `"trace" -> ggdraws_trace`.
#'   - `"density" -> ggdraws_dens`.
#'   - `"acf" -> ggdraws_acf`.
#'   - `"raster" -> ggdraws_raster`.
