#' Create Plotting Table (`drawsvis`)
#'
#' Generic used to convert an object to a `drawsvis` tibble.
#'
#' @param x object used to select a method.
#' @template param-ggdraws
#'
#' @name drawsvis
drawsvis <- function(x, .ggdraws, ...) {
  UseMethod("drawsvis")
}


#' Create Plotting Table from `draws` object
#'
#' Convert a grouped tibble holding MCMC samples into a visualisation table.
#'
#' @template param-draws
#' @templateVar draws_arg x
#' @template param-ggdraws
#'
#' @name draws-to-drawsvis
#' @export
drawsvis.draws <- function(x, .ggdraws, ...) {
  .ggdraws <- parse_ggdraws(.ggdraws)

  draws %>%
    dplyr::group_by(.data$.chain, .add = TRUE) %>%
    dplyr::summarise(
      .plot = list(.ggdraws(dplyr::cur_data_all(), ...)),
      .groups = "keep"
    ) %>%
    dplyr::arrange(.data$.chain) %>%
    dplyr::relocate(.data$.chain, .before = 1)
}
