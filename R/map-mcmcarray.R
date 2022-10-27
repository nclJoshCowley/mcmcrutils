utils::globalVariables("!<-")

#' Apply function to each MCMC Draw
#'
#' Recreation of purrr's `map`-like functionality for `mcmcarray`(s).
#'
#' @param x,y,l [`mcmcarray`][mcmcr::mcmcarray-object()] object(s), `l` is a
#'   list of such object(s).
#' @param .f function or formula. See [rlang::as_function()].
#' @param ... extra arguments passed to `.f`
#'
#' @return [`mcmcarray`][mcmcr::mcmcarray-object()] object, identical iteration
#'   and chain dimensions, storing output of `.f(l, ...)` or similar.
#'
#' @name map_mcmcarray
NULL


#' @rdname map_mcmcarray
#' @export
map_mcmcarray <- function(x, .f, ...) pmap_mcmcarray(list(x), .f, ...)


#' @rdname map_mcmcarray
#' @export
map2_mcmcarray <- function(x, y, .f, ...) pmap_mcmcarray(list(x, y), .f, ...)


#' @rdname map_mcmcarray
#' @export
pmap_mcmcarray <- function(l, .f, ...) {
  l_fail <- Filter(Negate(mcmcr::is.mcmcarray), l)
  if (length(l_fail)) stop("Non-mcmcarray objects: ", toString(names(l_fail)))

  .f <- rlang::as_function(.f)

  ni <- unique(vapply(l, mcmcr::niters, integer(1)))
  nc <- unique(vapply(l, mcmcr::nchains, integer(1)))

  stopifnot(
    "Number of iterations conflict" = (length(ni) == 1),
    "Number of chains conflict" = (length(nc) == 1)
  )

  # Need to perform calculation once to get an idea of dimensions
  l_proto <- lapply(l, extract_single_draw, chain = 1, iter = 1)
  out_proto <- do.call(.f, c(l_proto, ...))
  out_dims <- mcmcr::dims(out_proto)

  out <- array(NA, dim = c(nc, ni, out_dims))
  class(out) <- "mcmcarray"

  # Non-standard assignment as unsure on dimensions of `out`
  lhs_expr <- quote(out[ic, ii, ])[c(1:4, rep(5, length(out_dims)))]

  for (ic in seq_len(nc)) {
    for (ii in seq_len(ni)) {
      current_l <- lapply(l, extract_single_draw, chain = ic, iter = ii)
      eval(rlang::expr(!!lhs_expr <- do.call(.f, c(current_l, ...))))
    }
  }

  return(out)
}


#' Extract Single Draw
#'
#' Drops MCMC dimensions without `drop = TRUE` over the whole array.
#'   Useful for looping, as done in [map_mcmcarray()].
#'
#' @param x `mcmcarray` object with indexing `[chain, iteration, ...]`.
#' @param chain,iter integer. MCMC indexes to extract from.
#'
#' @keywords internal
#' @noRd
extract_single_draw <- function(x, chain, iter) {
  d <- dim(x)

  xic <- rlang::expr(x[!!chain, !!iter, , drop = FALSE])
  xic <- xic[c(1:4, rep(5, length(d) - 2), 6)]

  out <- eval(xic)
  dim(out) <- d[-c(1, 2)]

  return(out)
}
