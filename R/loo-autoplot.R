#' @importFrom ggplot2 autoplot
#' @export
ggplot2::autoplot


#' Diagnostic Plot(s) for PSIS in ggplot2
#'
#' A ggplot2 recreation of the [`plot()`][loo::pareto-k-diagnostic] methods
#'   for `loo` package objects.
#'
#' @param object object created by [loo::loo()] or [loo::psis()].
#' @param diagnostic choice. Should the Pareto K estimates or PSIS effective
#'   sample size be plotted.
#' @param ... Currently unused, may be forwarded to a `geom` in the future.
#'
#' @export
autoplot.loo <- function(object, diagnostic = c("k", "n_eff"), ...) {
  diagnostic <- match.arg(diagnostic)

  if (diagnostic == "k") {
    # Diagnostic 1: Pareto shape k
    k <- object$diagnostics[["pareto_k"]] %||% object[["pareto_k"]]
    if (is.null(k)) stop("No Pareto k estimates found.", call. = FALSE)
    k[is.na(k)] <- 0
    tb <- tibble::enframe(k, name = ".obs", value = "k")

  } else {
    # Diagnostic 2: n_eff
    n_eff <- object$diagnostics[["n_eff"]]
    if (is.null(k)) stop("No PSIS n_eff estimates found.", call. = FALSE)
    tb <- tibble::enframe(n_eff, name = ".obs", value = "n_eff")
  }

  out <-
    tb %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$.obs, y = .data[[diagnostic]])) +
    ggplot2::geom_point(alpha = 0.8, shape = 3, size = 2) +
    ggplot2::labs(
      y = if (diagnostic == "k") "Pareto Shape Parameter" else "PSIS n_eff",
      x = "Observation"
    )

  if (diagnostic == "n_eff") return(out)

  out +
    ggplot2::geom_hline(yintercept = 0.7, linetype = "longdash", alpha = 0.4) +
    ggplot2::expand_limits(y = c(-0.25, 1))
}
