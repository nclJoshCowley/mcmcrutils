#' MCMC Array to Prediction Tibble
#'
#' Convert `n`-length MCMC array into nested predictions where each tibble in
#'   the nest contains `.chain` and `.pred`, `.pred_lower`, `.pred_upper`.
#'
#' @param x [`mcmcarray`][mcmcr::mcmcarray-object]. Representing predictions.
#' @inheritParams draws_summarise
#'
#' @export
mcmcarray_to_pred <- function(x, collapse_chains = FALSE) {
  draws_summary <-
    mcmcrutils::draws(x) %>%
    mcmcrutils::draws_summarise(collapse_chains = collapse_chains) %>%
    dplyr::rename(
      .pred = ".value", .pred_lower = ".lower",.pred_upper = ".upper"
    )

  draws_summary %>%
    dplyr::group_by(.data$.term) %>%
    dplyr::group_nest(.key = ".pred", keep = FALSE) %>%
    dplyr::select(".pred")
}
