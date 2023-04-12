#' Draws, Add a 'Truth' Column
#'
#' Adds column based on a supplied R object for comparison.
#'
#' @template param-draws
#' @param ... Name-value pairs to be joined to the table.
#' @param .list list. Alternative way of passing arguments to `...`.
#'
#' @export
draws_add_truth <- function(draws, ..., .list = NULL) {
  args <- c(rlang::list2(...), .list)
  nms <- names(args)

  if (is.null(nms) || any(vapply(nms, Negate(nzchar), logical(1)))) {
    stop("All of ... must have names that are non-empty strings")
  }

  # Warn about discrepancy between draws and truth
  issue_nms <- setdiff(nms, mcmcr::pars(draws$.term))
  arg_dims <- lapply(args, function(.x) dim(.x) %||% length(.x))

  if (length(issue_nms)) {
    warning("Varaibles missing from draws: ", toString(issue_nms))
  } else if (!identical(arg_dims, mcmcr::pdims(draws$.term))) {
    warning("Dimension mismatch between truth and draws")
  }

  truth_tb <-
    dplyr::bind_rows(mapply(enterm, args, nms, ".truth", SIMPLIFY = FALSE))

  return(dplyr::left_join(draws, truth_tb, by = ".term"))
}


#' Draws, Separate Chains
#'
#' Split Draws-type tibble into a list of `draws` for chain-by-chain
#'   manipulation.
#'
#' @template param-draws
#' @param .keep logical. When `TRUE`, the chain column is kept in the output.
#'
#' @export
draws_separate_chains <- function(draws, .keep = FALSE) {
  out <- split(draws, draws$.chain)
  names(out) <- paste("Chain", seq_along(out))

  if (.keep) return(out)

  return(lapply(out, function(.x) .x[-match(".chain", names(.x))]))
}


#' Draws, Summarise
#'
#' Calculate point and interval summaries of draws.
#'
#' @template param-draws
#' @param .point function. Point estimate method, defaults to [`mean`].
#' @param .width numeric. Width of CI to be calculated with [`stats::quantile`].
#' @param collapse_chains logical. Single estimate for all chains when `TRUE`.
#'
#' @seealso [ggdist::point_interval()] for inspiration.
#'
#' @export
draws_summarise <- function(draws, .point = mean, .width = 0.95, collapse_chains = FALSE) {
  draws <- dplyr::group_by(draws, .data$.chain, .data$.term, .add = TRUE)

  if (collapse_chains) draws <- dplyr::ungroup(draws, .data$.chain)

  ci <- isFALSE(is.null(.width))
  if (ci) stopifnot(".width should be in [0, 1]" = 0 <= .width, .width <= 1)

  out <-
    draws |>
    dplyr::summarise(
      .lower = if (ci) {
        stats::quantile(.data$.value, probs = mean(c(0, 1 - .width)))
      },
      .upper = if (ci) {
        stats::quantile(.data$.value, probs = mean(c(1, .width)))
      },
      .value = .point(.data$.value),
      .groups = "keep"
    ) |>
    dplyr::relocate(".value", .before = ".lower")

  if (any(dplyr::group_size(out) != 1)) {
    warning("Multiple rows returned for single groups", call. = FALSE)
  }

  return(out)
}
















#' Draws, Unnest Term
#'
#' Splits the `.term` column of a draws object into a data frame column.
#'
#' @template param-draws
#'
#' @export
draws_unnest_term <- function(draws) {
  draws_per_par <- split(draws, term::pars_terms(draws$.term))

  unnested_draws <-
    lapply(draws_per_par, function(.x) {
      .x$.term <-
        tibble::tibble(
          par = term::pars(.x$.term),
          term_dims_to_tibble(.x$.term)
        )

      return(.x)
    })

  return(dplyr::bind_rows(unnested_draws))
}
