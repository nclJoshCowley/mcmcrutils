#' Draws, Add a 'Truth' Column
#'
#' Adds column based on a supplied R object for comparison.
#'
#' @template draws-method
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
