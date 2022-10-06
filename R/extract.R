#' @keywords internal
coef_trans <- function(coef_names) {
  coef_names |> stringr::str_replace_all(": ", "=")
}

#' Extract fixed-effects estimates
#'
#' Extract the fixed-effects estimates from a `jglmm` object.
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
#' Extract the variance-covariance matrix of the main parameters from a
#' `jglmm` object.
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
#' `jglmm` object.
#'
#' @importFrom stats sigma
#' @param x An object of class `jglmm`, as returned by `jglmm()`.
#'
#' @return Estimate of σ, the standard deviation of the per-observation noise.
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
#' Extract the conditional modes of the random effects from a `jglmm` object.
#'
#' @importFrom lme4 ranef
#' @param x An object of class `jglmm`, as returned by `jglmm()`.
#'
#' @return A list of data frames, one for each grouping factor for the random
#'   effects. The number of rows in the data frame is the number of levels of
#'   the grouping factor. The number of columns is the dimension of the random
#'   effect associated with each level of the factor. Each of the data frames
#'   has an attribute called "postVar", which contains an array for each
#'   random-effects term with the variance-covariance matrices for each level of
#'   the grouping factor.
#' @export
#'
#' @examples
#' \dontrun{
#' jglmm_setup()
#' cbpp <- dplyr::mutate(lme4::cbpp, prop = incidence / size)
#' gm <- jglmm(prop ~ period + (1 | herd), data = cbpp, family = "binomial",
#'             weights = cbpp$size)
#' ranef(gm)
#' }
ranef.jglmm <- function(x) {
  julia_assign("model", x$model)
  ranef_terms <- julia_eval("keys(model_ranef)")
  model_ranef <- julia_eval("model_ranef = map(DataFrame, raneftables(model));")
  cond_var <- julia_eval("condVar(model)")
  purrr::map(1:length(model_ranef), \(i) {
    df <- dplyr::as_tibble(model_ranef[[i]]) |> dplyr::rename_with(coef_trans)
    attr(df, "postVar") <- cond_var[[i]]
    return(df)
  }) |>
    purrr::set_names(ranef_terms)
}


#' Extract model fitted values
#'
#' Extract the fitted values from a `jglmm` object.
#'
#' @importFrom stats fitted
#' @param x An object of class `jglmm`, as returned by `jglmm()`.
#'
#' @return Vector of fitted values extracted from the model.
#' @export
#'
#' @examples
#' \dontrun{
#' jglmm_setup()
#' cbpp <- dplyr::mutate(lme4::cbpp, prop = incidence / size)
#' gm <- jglmm(prop ~ period + (1 | herd), data = cbpp, family = "binomial",
#'             weights = cbpp$size)
#' fitted(gm)
#' }
fitted.jglmm <- function(x) {
  julia_assign("model", x$model)
  julia_eval("fitted(model)")
}
