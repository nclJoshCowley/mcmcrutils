#' Draw Prior Information
#'
#' Convenience wrapper around [ggplot2::stat_function] to show prior densities.
#'
#' @param fun function. Prior function to use (see [rlang::as_function]).
#' @param ... passed to `fun`.
#' @param layer_args list. Override visual defaults passed to
#'   [ggplot2::stat_function].
#'
#' @export
stat_prior <- function(fun, ..., layer_args = list()) {
  sf_args <-
    utils::modifyList(
      list(
        fun = rlang::as_function(fun),
        args = rlang::list2(...),
        linetype = "dashed",
        alpha = 0.6
      ),
      layer_args
    )

  do.call(ggplot2::stat_function, sf_args)
}
