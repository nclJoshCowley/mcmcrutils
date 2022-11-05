#' Drawsvis Methods
#'
#' @template param-drawsvis
#' @param ... Modifiers where position implies dimension.
#'   - Character vectors are used as extractor functions on each index.
#'   - Functions or formulas are applied to each index.
#'   - Explicit missing arguments mean that dimension is omitted from the label.
#'   - `NULL` values assert the default and indices are used as integers.
#'
#' @export
drawsvis_add_term_subtitle <- function(dv, ...) {
  if (mcmcr::npars(dv$.term) > 1) stop("Not implemented for multiple terms")
  np_dims <- mcmcr::npdims(dv$.term)[[1]]

  # Need to apply `dots` as modifiers to `dnames` (defaults to numeric)
  dots <- rlang::dots_list(..., .ignore_empty = "none", .preserve_empty = TRUE)
  dnames <- term_dims_to_tibble(dv$.term)

  if (length(dots) != np_dims) {
    stop("Required ", np_dims, " arguments passed to ...")
  }

  # Missing => Exclude
  is_included <- vapply(dots, Negate(rlang::is_missing), logical(1))
  dots <- dots[is_included]
  dnames <- dnames[is_included]

  for (i in seq_along(dots)) {
    # Character argument => extractor function
    if (is.character(dots[[i]])) dnames[[i]] <- dots[[i]][dnames[[i]]]

    # Function / formula => apply as_function
    if (is.function(dots[[i]]) || rlang::is_formula(dots[[i]])) {
      dnames[[i]] <- rlang::as_function(dots[[i]])(dnames[[i]])
    }
  }

  label_strings <- do.call(paste, c(dnames, sep = ", "))
  label_objects <- Map(ggplot2::labs, subtitle = label_strings)

  dv$.plot <- Map(`+`, dv$.plot, label_objects)

  return(dv)
}
