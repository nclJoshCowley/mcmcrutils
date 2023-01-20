#' Create JAGS Algorithm Controls
#'
#' Named list to control how long JAGS samples from the posterior.
#'
#' @param n.adapt integer. Number of samples discarded in adaptation stage;
#' @param n.update integer. Number of samples discarded in warm-up stage;
#' @param n.iter integer. Number of samples stored from the monitoring stage;
#' @param n.thin integer. Thinning interval, only every *n.thin*-th sample kept;
#' @param n.chains integer. Number of chains to create.
#'
#' @return Object with S3 class `jags_n` that can be used as a normal `list`.
#'
#' @export
jags_n <- function(n.adapt, n.update, n.iter, n.thin, n.chains) {
  # Currently no validation of values
  out <-
    list(
      n.adapt = n.adapt, n.update = n.update, n.iter = n.iter,
      n.thin = n.thin, n.chains = n.chains
    )

  return(structure(out, class = "jags_n"))
}


#' Show JAGS Controls
#'
#' @param x object of [`jags_n`] type.
#' @template generic-dots
#'
#' @export
print.jags_n <- function(x, ...) {
  pretty_x <- lapply(x, prettyNum, big.mark = ",")

  out <-
    with(pretty_x, paste0(
      sprintf("%s chains:", n.chains),
      sprintf(" %s -> %s -> %s", n.adapt, n.update, n.iter),
      if (x$n.thin != 1) sprintf(" (thinned by %s)", n.thin)
    ))

  print(out)
  invisible(out)
}
