#' Extract Log-Likelihood
#'
#' @importFrom stats logLik
#' @param x An object of class `jglmm`, as returned by `jglmm`.
#'
#' @return An object of class \code{logLik}, a number with attribute \code{"df"}
#'   (\strong{d}egrees of \strong{f}reedom), giving the number of (estimated)
#'   parameters in the model.
#' @export
#'
#' @examples
#' \dontrun{
#' jglmm_setup()
#' lm1 <- jglmm(Reaction ~ Days + (Days | Subject), lme4::sleepstudy)
#' logLik(lm1)
#' }
logLik.jglmm <- function(x) {
  julia_assign("model", x$model)
  loglik <- julia_eval("loglikelihood(model);")
  attr(loglik, "df") <- julia_eval("dof(model);")
  class(loglik) <- "logLik"
  loglik
}

#' Extract AIC from a Fitted Model
#'
#' @importFrom stats extractAIC
#' @param x An object of class `jglmm`, as returned by `jglmm`.
#' @param k Numeric specifying the 'weight' of the
#'    \emph{degrees of freedom} part in the AIC formula.
#'
#' @return A numeric vector of length 2, with first and second elements giving
#'
#'    `df` the '\strong{d}egrees of \strong{f}reedom' for the fitted model in
#'    `x`.
#'
#'    `AIC` the (generalized) Akaike Information Criterion for the fitted model
#'    in `x`.
#' @export
#'
#' @examples
#' \dontrun{
#' jglmm_setup()
#' lm1 <- jglmm(Reaction ~ Days + (Days | Subject), lme4::sleepstudy)
#' aic <- extractAIC(lm1)
#' bic <- extractAIC(lm1, k = log(nobs(lm1)))
#' }
extractAIC.jglmm <- function(x, k = 2) {
  julia_assign("model", x$model)
  df <- julia_eval("dof(model);")
  loglik <- julia_eval("loglikelihood(model);")
  aic = -2 * loglik + k * df
  c(df, aic)
}

#' Extract the Number of Observations from a Fit
#'
#' @importFrom stats nobs
#' @param x An object of class `jglmm`, as returned by `jglmm`.
#'
#' @return A numeric giving the number of observations.
#' @export
#'
#' @examples
#' \dontrun{
#' jglmm_setup()
#' lm1 <- jglmm(Reaction ~ Days + (Days | Subject), lme4::sleepstudy)
#' nobs(lm1)
#' }
nobs.jglmm <- function(x) {
  julia_assign("model", x$model)
  julia_eval("nobs(model);")
}

#' Model Deviance
#'
#' @importFrom stats deviance
#' @param x An object of class `jglmm`, as returned by `jglmm`.
#'
#' @return A numeric giving the deviance extracted from the fitted model.
#' @export
#'
#' @examples
#' \dontrun{
#' jglmm_setup()
#' lm1 <- jglmm(Reaction ~ Days + (Days | Subject), lme4::sleepstudy)
#' deviance(lm1)
#' }
deviance.jglmm <- function(x) {
  julia_assign("model", x$model)
  # NOTE: for GLMs, there is some adjustment for the saturated model, although
  # this differs from the adjustment made by lme4::deviance.merMod, resulting
  # in different values. To be investigated in the future.
  julia_eval("deviance(model);")
}