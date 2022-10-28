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
  new_map <- modify_aes(x = .data$.iteration, y = .data$.value, user = mapping)

  ggplot2::ggplot(draws) +
    ggplot2::geom_line(mapping = new_map, ...) +
    mcmcrutils::scale_x_iterations(name = NULL) +
    ggplot2::labs(x = NULL, y = NULL)
}

#' @rdname ggdraws
#' @export
ggdraws_dens <- function(draws, mapping = NULL, ...) {
  new_map <- modify_aes(x = .data$.value, user = mapping)

  new_args <-
    utils::modifyList(list(mapping = new_map, alpha = 0.1), rlang::list2(...))

  ggplot2::ggplot(draws) +
    do.call(ggplot2::geom_density, new_args) +
    ggplot2::labs(x = NULL, y = NULL)
}


#' @rdname ggdraws
#'
#' @param lag.max integer. Passed to [`acf`][stats::acf()].
#'
#' @export
ggdraws_acf <- function(draws, mapping = NULL, ..., lag.max) {
  if (missing(lag.max)) lag.max <- NULL

  new_map <- modify_aes(x = .data$lag, y = .data$acf, user = mapping)

  new_args <-
    utils::modifyList(
      list(
        mapping = new_map, stat = "identity", position = "dodge", width = 0.2
      ),
      rlang::list2(...)
    )

  get_acf_as_df <- function(.x) {
    acf_obj <- stats::acf(.x, lag.max = lag.max, plot = FALSE)
    out <- unclass(acf_obj)[c("acf", "lag")]
    return(as.data.frame(out))
  }

  draws_acf <-
    draws %>%
    dplyr::group_by(.data$.term, !!new_map$fill) %>%
    dplyr::summarise(get_acf_as_df(.data$.value), .groups = "drop")

  ggplot2::ggplot(draws_acf) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = 0)) +
    do.call(ggplot2::geom_bar, new_args) +
    ggplot2::labs(y = "ACF", x = NULL)
}


#' @rdname ggdraws
#'
#' @param levels vector. Range of possible values needed when `.value` is
#'   converted to a factor.
#'
#' @export
ggdraws_raster <- function(draws, mapping = NULL, ..., levels) {
  if (missing(levels)) levels <- seq(min(draws$.value), max(draws$.value))

  new_map <-
    modify_aes(
      x = .data$.iteration,
      y = map_tindex(.data$.term, 1),
      fill = factor(.data$.value, levels = levels),
      user = mapping
    )

  draws %>%
    ggplot2::ggplot() +
    ggplot2::geom_tile(mapping = new_map) +
    mcmcrutils::scale_x_iterations() +
    ggplot2::scale_fill_discrete(name = NULL, drop = FALSE) +
    ggplot2::labs(y = "Index")
}


#' Modify `aes`
#'
#' Set mapping defaults and update with user input, if supplied
#'
#' @param ... developer supplied defaults.
#' @param user list. User supplied mappings.
#'
#' @keywords internal
modify_aes <- function(..., user) {
  if (is.null(user)) user <- list(NULL)
  return(utils::modifyList(ggplot2::aes(...), val = user))
}
