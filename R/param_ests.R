#' @keywords internal
coef_trans <- function(coef_names) {
  coef_names |> stringr::str_remove(": ")
}

#' Extract fixed-effects estimates
#'
#' @importFrom lme4 fixef
#' @param x An object of class `jglmm`, as returned by `jglmm()`.
#'
#' @return A named numeric vector of fixed-effects estimates.
#' @export
#'
#' @examples
#' \dontrun{
#' jglmm_setup()
#' lm1 <- jglmm(Reaction ~ Days + (Days | Subject), lme4::sleepstudy)
#' fixef(lm1)
#' }
fixef.jglmm <- function(x) {
  julia_assign("model", x$model)
  fixef_vals <- julia_eval("fixef(model);")
  fixef_names <- julia_eval("fixefnames(model);") |> coef_trans()
  rlang::set_names(fixef_vals, fixef_names)
}

#' Calculate variance-covariance matrix for a fitted model object
#'
#' @importFrom stats vcov
#' @param x An object of class `jglmm`, as returned by `jglmm()`.
#'
#' @return A matrix of the estimated covariances between the parameter estimates
#'   in the linear or non-linear predictor of the model.
#' @export
#'
#' @examples
#' \dontrun{
#' jglmm_setup()
#' lm1 <- jglmm(Reaction ~ Days + (Days | Subject), lme4::sleepstudy)
#' vcov(lm1)
#' }
vcov.jglmm <- function(x) {
  julia_assign("model", x$model)
  vcov_matrix <- julia_eval("vcov(model);")
  coef_names <- julia_eval("coefnames(model);") |> coef_trans()
  colnames(vcov_matrix) <- coef_names
  rownames(vcov_matrix) <- coef_names
  return(vcov_matrix)
}

#' Extract residual standard deviation 'sigma'
#'
#' Extract the estimated standard deviation of the errors, the "residual
#' standard deviation" (also misnamed the "residual standard error"), from a
#' fitted model.
#'
#' @importFrom stats sigma
#' @param x An object of class `jglmm`, as returned by `jglmm()`.
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' jglmm_setup()
#' lm1 <- jglmm(Reaction ~ Days + (Days | Subject), lme4::sleepstudy)
#' sigma(lm1)
#' }
sigma.jglmm <- function(x) {
  julia_assign("model", x$model)
  julia_eval("sdest(model);")
}

#' Extract the modes of the random effects
#'
#' Extract the conditional modes of the random effects from a fitted `jglmm`
#' object.
#'
#' @param x An object of class `jglmm`, as returned by `jglmm()`.
#'
#' @return A list of tibbles, one for each random effect group.
#' @export
#'
#' @examples
#' \dontrun{
#' jglmm_setup()
#' cbpp <- dplyr::mutate(lme4::cbpp, prop = incidence / size)
#' gm <- jglmm(prop ~ period + (1 | herd), data = cbpp, family = "binomial",
#'             weights = cbpp$size)
#' ranef_jglmm(gm)
#' }
ranef_jglmm <- function(x) {
  julia_assign("model", x$model)
  julia_command("model_ranef = map(DataFrame, raneftables(model));")
  model_ranef <- julia_eval("model_ranef")
  ranef_terms <- julia_eval("keys(model_ranef)")
  purrr::map(1:length(model_ranef), ~dplyr::as_tibble(model_ranef[.x])) %>%
    purrr::set_names(ranef_terms)
}
