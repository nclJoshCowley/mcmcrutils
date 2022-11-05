#' Draws, Summarise
#'
#' Wrapper around [`point_interval`][ggdist::point_interval()] to calculate
#'   point and interval summaries of draws.
#'
#' @template param-draws
#' @param ... passed to [`point_interval()`][ggdist::point_interval()].
#' @param .point function. Point estimate method (default changed to mean).
#'
#' @export
draws_summarise <- function(draws, ..., .point = mean) {
  args_default <-
    list(
      .data = dplyr::group_by(draws, .data$.chain, .data$.term),
      .point = .point
    )

  args_updated <- utils::modifyList(args_default, rlang::list2(...))

  out <- do.call(ggdist::point_interval, args_updated)
  out[c(".width", ".point", ".interval")] <- NULL
  return(out)
}
