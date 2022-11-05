#' @param <%=get0("draws_arg", ifnotfound = "draws")%> See **Draws** section below.
#'
#' @family draws methods
#'
#' @section Draws:
#'   Typically output from [gather_draws_by_term()], a `draws`-style
#'     [tibble][tibble::tibble()] assumed to have columns:
#'
#'   - `.chain`, `.iteration`, `.draw`. MCMC sample indices.
#'   - `.term`. Parameter information.
#'   - `.value`. MCMC sample value.
