#' LOO Cross Validation within `mcmcr`
#'
#' Methods for [`loo()`][loo::loo-package] package functionality.
#'
#' @param x [`mcmcarray`][mcmcr::mcmcarray-object()] object. Log-likelihood.
#' @template generic-dots
#'
#' @seealso LPD (see references), PSIS ([loo::loo()]),
#'   WAIC ([loo::waic()]).
#'
#' @references
#'   Vehtari et al. (2017), <https://doi.org/10.1007/s11222-016-9696-4>.
#'
#' @name loo-mixexpert
NULL


#' Log Pointwise Predictive Density
#'
#' @describeIn loo-mixexpert
#'   Computes simulation-estimate for log pointwise predictive density (LPD).
#'
#' @export
log_pred_density <- function(x) {
  lik <- mcmcrutils::map_mcmcarray(x, exp)

  # See Vehtari (2017), Equation 3.
  # LPD = sum_i log( likelihood_mcmc_mean )
  #     = sum_i log( 1/S sum_s { new_lik })
  likelihood_mcmc_mean <- mcmcrutils::estimates_per_chain(lik, mean)
  out <- vapply(likelihood_mcmc_mean, function(.x) sum(log(.x)), double(1))

  return(structure(out, .Names = sprintf("Chain %s", seq_along(out))))
}


#' @rdname loo-mixexpert
#' @export
waic.mcmcarray <- function(x, ...) {
  out <- lapply(asplit(x, 1), loo::waic, ...)
  return(structure(out, .Names = sprintf("Chain %s", seq_along(out))))
}


#' @rdname loo-mixexpert
#' @export
loo.mcmcarray <- function(x, ...) {
  out <- lapply(asplit(x, 1), loo::loo, ...)
  return(structure(out, .Names = sprintf("Chain %s", seq_along(out))))
}


#' Compare Models using Log Likelihood
#'
#' High-level wrapper around chosen comparison metrics, calculated per chain.
#'
#' @param loglik_list named list. Each element is log likelihood of the model,
#'   stored in a [`mcmcarray`][mcmcr::mcmcarray-object].
#' @param cols character. Columns to be used in [loo::loo_compare()].
#' @param type character choice. Defines output type.
#'
#' @export
compare_via_loglik <- function(loglik_list, cols, type = c("raw", "gt")) {
  if (is.null(names(loglik_list)) || any(duplicated(names(loglik_list)))) {
    stop("Input list needs unique names")
  }

  if (missing(cols)) cols <- c("elpd_diff", "se_diff")

  lpds <- lapply(loglik_list, log_pred_density)

  lpd_comp <-
    lpds %>%
    purrr::transpose() %>%
    lapply(tibble::enframe, name = "model", value = "lpd") %>%
    lapply(dplyr::mutate, lpd = unlist(.data$lpd)) %>%
    lapply(dplyr::mutate, lpd = .data$lpd - max(.data$lpd))


  # Want `ll_per_chain[[i]][[j]]` to be loglik matrix for chain i, model j.
  ll_per_chain <- purrr::transpose(lapply(loglik_list, asplit, 1))

  waics <- purrr::map_depth(ll_per_chain, 2, loo::waic)
  waic_comp <- lapply(waics, loo::loo_compare)

  loos <- purrr::map_depth(ll_per_chain, 2, loo::loo)
  loo_comp <- lapply(loos, loo::loo_compare)

  waic_tb <-
    lapply(waic_comp, function(.x) tibble::as_tibble(
      .x[, cols],
      rownames = "model",
      .name_repair = ~ paste0("waic_", .x)
    ))

  loo_tb <-
    lapply(loo_comp, function(.x) tibble::as_tibble(
      .x[, cols],
      rownames = "model",
      .name_repair = ~ paste0("loo_", .x)
    ))

  loo_results <- Map(dplyr::full_join, waic_tb, loo_tb, by = "model")
  results <-
    Map(dplyr::full_join, lpd_comp, loo_results, by = "model") %>%
    lapply(dplyr::arrange, dplyr::desc(.data$waic_elpd_diff))

  if (match.arg(type) == "raw") return(results)

  lapply(results, function(.x) {
    .x %>%
      gt::gt(rowname_col = "model") %>%
      gt::fmt_number(gt::everything(), decimals = 2) %>%
      gt::cols_align("center") %>%
      gt::cols_width(gt::everything() ~ "30%") %>%
      gt::cols_label(
        lpd = gt::html("&Delta; LPD"),
        waic_elpd_diff = gt::html("&Delta; WAIC (Std. Error)"),
        loo_elpd_diff = gt::html("&Delta; PSIS (Std. Error)")
      ) %>%
      gt::cols_merge(
        c("waic_elpd_diff", "waic_se_diff"), pattern = "{1} ({2})"
      ) %>%
      gt::cols_merge(
        c("loo_elpd_diff", "loo_se_diff"), pattern = "{1} ({2})"
      )
  })
}
