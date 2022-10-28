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
