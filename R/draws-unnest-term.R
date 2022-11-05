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
