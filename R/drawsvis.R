#' Create `DrawsVis` tibble
#'
#' Nests an already grouped Draws-type tibble into a Drawsvis-type tibble
#'   with a list column of `ggplot2` objects.
#'
#' A generic, `build_drawsvis`, is supplied to be extended by modelling
#'   packages to convert any object directly into a Drawsvis-type tibble.
#'
#' @template draws-method
#' @template ggdraws-argument
#'
#' @name drawsvis
NULL


#' @rdname drawsvis
#' @export
drawsvis <- function(draws, .ggdraws, ...) {
  .ggdraws <- parse_ggdraws(.ggdraws)

  draws %>%
    dplyr::group_by(.data$.chain, .add = TRUE) %>%
    dplyr::summarise(
      .plot = list(.ggdraws(dplyr::cur_data(), ...)),
      .groups = "drop_last"
    ) %>%
    dplyr::arrange(.data$.chain) %>%
    dplyr::relocate(.data$.chain, .before = 1)
}


#' @rdname drawsvis
#' @export
build_drawsvis <- function(x, ...) {
  UseMethod("build_drawsvis")
}
