#' Position Scale for MCMC Iterations
#'
#' Wrapper around [`scale_x_continuous`][ggplot2::scale_x_continuous()] with
#'   updated defaults for thousands of MCMC iterations.
#'
#' @param ... passed to `scale_x_continuous`
#' @param n integer. Number of desirable breaks.
#'
#' @export
scale_x_iterations <- function(..., n = 5) {
  defaults <-
    list(
      name = "Iterations",
      breaks = function(x) {
        br <- pretty(x, n = n)
        br[1] <- 0
        all_br_gt_1e3 <- all(log10(subset(br, br != 0)) >= 3)

        # if (all_br_gt_1e3) br[length(br)] <- 1e3 * floor(x[2] / 1e3)
        # if (!all_br_gt_1e3) br[length(br)] <- 1e2 * floor(x[2] / 1e2)

        return(unique(sort(br)))
      },
      labels = function(br) {
        all_br_gt_1e3 <- all(log10(subset(br, br != 0)) >= 3)
        sprintf(
          if (all_br_gt_1e3) "%.0fk" else "%.1fk",
          br / 1e3
        )
      }
    )

  args <- utils::modifyList(defaults, rlang::list2(...))

  return(do.call(ggplot2::scale_x_continuous, args))
}
