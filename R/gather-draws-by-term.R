#' Extract Draws into a Tidy Data Format
#'
#' Mimics the behaviour of [tidybayes::gather_draws()] using a `.term` column
#'
#' @template param-mcmcarray
#' @param ... extra arguments ignored.
#'
#' @export
gather_draws_by_term <- function(x, ...) {
  UseMethod("gather_draws_by_term")
}


#' @rdname gather_draws_by_term
#' @param pars character vector. Names of parameters to subset by.
#' @export
gather_draws_by_term.mcmcr <- function(x, pars = NULL, ...) {
  mcmcr::chk_mcmcr(x)

  if (isFALSE(is.null(pars))) {
    pars_missing <- setdiff(pars, names(x))
    if (length(pars_missing)) stop("Couldn't find pars = ", toString(pars))
    x <- x[pars]
  }

  mapply(gather_draws_by_term.mcmcarray, x, names(x), SIMPLIFY = FALSE)
}


#' @rdname gather_draws_by_term
#' @param name character. Parameter name to be assumed for the `.term` column.
#' @export
gather_draws_by_term.mcmcarray <- function(x, name = "par", ...) {
  mcmcr::chk_mcmcarray(x)
  ni <- mcmcr::niters(x)

  # Need to make chain / iteration explicit
  x_dims <- which(array(TRUE, dim = dim(x)), arr.ind = TRUE)

  # Parameter dimensions to be stored as character in term, e.g. beta[2,2].
  x_pdims <- x_dims[, -c(1, 2), drop = FALSE]
  x_pdims_str <- do.call(paste, c(asplit(x_pdims, 2), list(sep = ",")))
  x_term <- term::term(sprintf("%s[%s]", name, x_pdims_str))

  out <-
    tibble::tibble(
      .chain = x_dims[, 1],
      .iteration = x_dims[, 2],
      .draw = ((.data$.chain - 1) * ni) + .data$.iteration,
      .term = x_term,
      .value = c(x)
    )

  # Default ordering
  out <- out[order(unclass(out$.term), out$.chain, out$.iteration), ]

  # Default grouping
  out <- dplyr::group_by(out, .data$.chain, .data$.term)

  return(out)
}
