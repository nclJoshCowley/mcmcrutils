#' Create Plotting Table (`drawsvis`)
#'
#' Generic used to convert an object to a `drawsvis` tibble.
#'
#' @param x object used to select a method.
#' @template param-ggdraws
#'
#' @name drawsvis
#' @export
drawsvis <- function(x, .ggdraws, ...) {
  UseMethod("drawsvis")
}


#' @rdname drawsvis
#' @export
drawsvis.grouped_df <- function(x, .ggdraws, ...) {
  .ggdraws <- parse_ggdraws(.ggdraws)

  out <-
    x %>%
    dplyr::group_by(.data$.chain, .add = TRUE) %>%
    dplyr::summarise(
      .plot = list(.ggdraws(dplyr::cur_data_all(), ...)),
      .groups = "drop"
    ) %>%
    dplyr::arrange(.data$.chain) %>%
    dplyr::relocate(.data$.chain, .before = 1)

  class(out) <- c("drawsvis", class(out))
  return(out)
}
