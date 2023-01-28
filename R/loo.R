#' LOO Cross Validation within `mcmcr`
#'
#' Methods for [`loo()`][loo::loo-package] package functionality.
#'
#' @param x [`mcmcarray`][mcmcr::mcmcarray-object()] object. Log-likelihood.
#' @param separate_chains logical. Chain analysed independently when `TRUE`.
#' @param r_eff vector. Either `"auto"` to uses [loo::relative_eff()] or will
#'   be passed directly to [loo::loo()].
#' @inheritParams rlang::args_dots_empty
#'
#' @seealso LPD (see references), PSIS ([loo::loo()]),
#'   WAIC ([loo::waic()]).
#'
#' @references
#'   Vehtari et al. (2017), <https://doi.org/10.1007/s11222-016-9696-4>.
#'
#' @name loo-mcmcrutils
NULL


#' @rdname loo-mcmcrutils
#'
#' @details `log_pred_density()` is the simulation-estimate for log pointwise
#'   predictive density (LPD).
#'
#' @export
log_pred_density <- function(x, separate_chains = TRUE) {
  if (isFALSE(separate_chains)) x <- mcmcr::collapse_chains(x)

  # See Vehtari (2017), Equation 3.
  # LPD = sum_i log( likelihood_mcmc_mean )
  #     = sum_i log( 1/S sum_s { new_lik })
  likelihood_mcmc_mean <- mcmcrutils::estimates_per_chain(exp(x), mean)
  out <- lapply(likelihood_mcmc_mean, function(.x) sum(log(.x)))

  if (isFALSE(separate_chains)) return(out[[1]])

  return(structure(out, .Names = sprintf("Chain %s", seq_along(out))))
}


#' @rdname loo-mcmcrutils
#' @export
waic.mcmcarray <- function(x, separate_chains = TRUE, ...) {
  requireNamespace("loo")
  rlang::check_dots_empty()

  if (isFALSE(separate_chains)) return(loo::waic(aperm(x, c(2, 1, 3))))

  out <- lapply(asplit(x, 1), loo::waic)
  return(structure(out, .Names = sprintf("Chain %s", seq_along(out))))
}


#' @rdname loo-mcmcrutils
#' @export
loo.mcmcarray <- function(x, separate_chains = TRUE, r_eff = NULL, ...) {
  requireNamespace("loo")
  rlang::check_dots_empty()

  if (identical(r_eff, "auto")) {
    r_eff <- loo::relative_eff(aperm(exp(x), c(2, 1, 3)))
  }

  if (isFALSE(separate_chains)) {
    return(loo::loo(aperm(x, c(2, 1, 3)), r_eff = r_eff))
  }

  out <- lapply(asplit(x, 1), loo::loo, r_eff = r_eff)
  return(structure(out, .Names = sprintf("Chain %s", seq_along(out))))
}



#' LOO Summary
#'
#' @describeIn loo-mcmcrutils
#'   Creates a 1-row tibble of each LOO metric (LPD, WAIC, PSIS).
#'
#' @export
loo_summary <- function(x, separate_chains = TRUE, r_eff = NULL) {
  requireNamespace("loo")

  lpd <- mcmcrutils::log_pred_density(x, separate_chains = separate_chains)
  waic <- mcmcrutils::waic.mcmcarray(x, separate_chains = separate_chains)

  psis <-
    mcmcrutils::loo.mcmcarray(
      x, separate_chains = separate_chains, r_eff = r_eff
    )

  if (isFALSE(separate_chains)) {
    lpd <- list(lpd)
    waic <- list(waic)
    psis <- list(psis)
  }

  lpd_tb <- lapply(lpd, tibble::as_tibble_row, .name_repair = ~ "lpd_est")
  waic_tb <- lapply(waic, loo_to_tibble_row, nm = "waic")
  psis_tb <- lapply(psis, loo_to_tibble_row, nm = "psis")

  out <- purrr::pmap(list(lpd_tb, waic_tb, psis_tb), dplyr::bind_cols)

  if (isFALSE(separate_chains)) out[[1]] else out
}


#' @keywords internal
#' @noRd
loo_to_tibble_row <- function(loo_object, nm, r = 1) {
  tibble::tibble_row(
    "{nm}_est" := loo_object$estimates[r, "Estimate"],
    "{nm}_se" := loo_object$estimates[r, "SE"]
  )
}



#' LOO Comparison Summary
#'
#' @describeIn loo-mcmcrutils
#'   Creates a tibble with one row per model showing metric comparisons and
#'   comparsion of standard errors, see [`loo_compare`][loo::loo_compare()].
#'
#' @param loglik_list named list of [`mcmcarray`][mcmcr::mcmcarray-object()]
#'   objects representing different model's log-likelihood.
#'
#' @export
loo_compare_summary <- function(loglik_list, separate_chains = TRUE, r_eff = NULL) {
  if (is.null(names(loglik_list))) {
    names(loglik_list) <- paste0("model", seq_along(loglik_list))
  }

  if (isFALSE(separate_chains)) {
    loglik_list <- lapply(loglik_list, mcmcr::collapse_chains)
  }

  lpd_list <- purrr::transpose(lapply(loglik_list, log_pred_density))
  waic_list <- purrr::transpose(lapply(loglik_list, loo::waic))
  psis_list <- purrr::transpose(lapply(loglik_list, loo::loo, r_eff = r_eff))

  lpd_tb <-
    lapply(lpd_list, function(.x) {
      x_vec <- unlist(.x)
      tibble::tibble(model = names(.x), lpd_diff_est = x_vec - max(x_vec))
    })

  waic_tb <-
    waic_list %>%
    lapply(loo::loo_compare) %>%
    lapply(loo_compare_to_tibble, nm = "waic")

  psis_tb <-
    psis_list %>%
    lapply(loo::loo_compare) %>%
    lapply(loo_compare_to_tibble, nm = "psis")


  # Need to join over nested list for all three tables.
  # => dplyr::full_join(lpd_tb[[1]], waic_tb[[1]], ...) etc.
  out <-
    purrr::reduce(
      list(lpd_tb, waic_tb, psis_tb),
      ~ Map(dplyr::full_join, .x, .y, by = "model")
    )

  if (isFALSE(separate_chains)) out[[1]] else out
}


#' @keywords internal
#' @noRd
loo_compare_to_tibble <- function(loo_compare_object, nm) {
  tibble::as_tibble(
    loo_compare_object[, 1:2],
    rownames = "model",
    .name_repair = ~ paste(nm, "diff", c("est", "se"), sep = "_")
  )

}
