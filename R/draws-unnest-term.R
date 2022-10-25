#' Draws, Unnest Term
#'
#' Splits the `.term` column of a draws object into a data frame column.
#'
#' @template draws-method
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


#' Convert Term `dims` Field to Tibble
#'
#' @param x object of type [`term`][term::term()].
#'
#' @keywords internal
#' @noRd
term_dims_to_tibble <- function(x) {
  tibble::as_tibble(
    do.call(rbind, term::tindex(x)),
    .name_repair = function(.nm) paste0("dim", seq_along(.nm))
  )
}
