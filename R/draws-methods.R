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
#' Wrapper around [`point_interval`][ggdist::point_interval()] to calculate
#'   point and interval summaries of draws.
#'
#' @template param-draws
#' @param ... passed to [`point_interval()`][ggdist::point_interval()].
#' @param .point function. Point estimate method (default changed to mean).
#' @param collapse_chains logical. Single estimate for all chains when `TRUE`.
#'
#' @export
draws_summarise <- function(draws, ..., .point = mean, collapse_chains = FALSE) {
  .data <- dplyr::group_by(draws, .data$.chain, .data$.term)

  if (collapse_chains) .data <- dplyr::ungroup(.data, .data$.chain)

  args_default <- list(.data = .data, .point = .point)

  args_updated <- utils::modifyList(args_default, rlang::list2(...))

  out <- do.call(ggdist::point_interval, args_updated)
  out[c(".width", ".point", ".interval")] <- NULL
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
