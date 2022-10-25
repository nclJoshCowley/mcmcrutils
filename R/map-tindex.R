#' Map `tindex()`
#'
#' Extract specific elements in the list returned by [term::tindex()].
#'
#' @param x object of type [`term`][term::term()].
#' @param margin integer. Which index to extract.
#'
#' @examples
#' map_tindex(term::term("term[10,1]", "term[20,1]"), 1)
#' #> term[1,1] term[2,1]
#' #>        10        20
#'
#' @export
map_tindex <- function(x, margin) {
  if (margin > term::npdims(x)) stop("'margin' exceeds term dimension")

  # Equiv. to purrr::map_int(term::tindex(x), `[`, margin)
  vapply(term::tindex(x), `[`, margin, FUN.VALUE = integer(1))
}
