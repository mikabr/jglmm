#' Extract Log-Likelihood
#'
#' @importFrom stats logLik
#' @param object An object of class `jglmm`, as returned by `jglmm()`.
#' @param ... Optional additional arguments, currently none are used.
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
logLik.jglmm <- function(object, ...) {
  julia_assign("model", object$model)
  loglik <- julia_eval("loglikelihood(model);")
  attr(loglik, "df") <- julia_eval("dof(model);")
  class(loglik) <- "logLik"
  loglik
}

#' Extract AIC from a Fitted Model
#'
#' @importFrom stats extractAIC
#' @param fit An object of class `jglmm`, as returned by `jglmm`.
#' @param scale Not currently used (see `extractAIC` generic).
#' @param k Numeric specifying the 'weight' of the
#'    \emph{degrees of freedom} part in the AIC formula.
#' @param ... Optional additional arguments, currently none are used.
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
extractAIC.jglmm <- function(fit, scale = 0, k = 2, ...) {
  julia_assign("model", fit$model)
  df <- julia_eval("dof(model);")
  loglik <- julia_eval("loglikelihood(model);")
  aic = -2 * loglik + k * df
  c(df, aic)
}

#' Extract the Number of Observations from a Fit
#'
#' @importFrom stats nobs
#' @param object An object of class `jglmm`, as returned by `jglmm()`.
#' @param ... Optional additional arguments, currently none are used.
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
nobs.jglmm <- function(object, ...) {
  julia_assign("model", object$model)
  julia_eval("nobs(model);")
}

#' Model Deviance
#'
#' @importFrom stats deviance
#' @param object An object of class `jglmm`, as returned by `jglmm()`.
#' @param ... Optional additional arguments, currently none are used.
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
deviance.jglmm <- function(object, ...) {
  julia_assign("model", object$model)
  # NOTE: for GLMs, there is some adjustment for the saturated model, although
  # this differs from the adjustment made by lme4::deviance.merMod, resulting
  # in different values. To be investigated in the future.
  julia_eval("deviance(model);")
}

opt_info <- function(object) {
  julia_assign("model", object$model)
  julia_command("os = model.optsum;")
  list(initial = julia_eval("os.initial"),
       finitial = julia_eval("os.finitial"),
       optimizer = julia_eval("os.optimizer"),
       lowerbd = julia_eval("os.lowerbd"),
       ftol_rel = julia_eval("os.ftol_rel"),
       ftol_abs = julia_eval("os.ftol_abs"),
       xtol_rel = julia_eval("os.xtol_rel"),
       xtol_abs = julia_eval("os.xtol_abs"),
       initial_step = julia_eval("os.initial_step"),
       maxfeval = julia_eval("os.maxfeval"),
       maxtime = julia_eval("os.maxtime"),
       feval = julia_eval("os.feval"),
       final = julia_eval("os.final"),
       fmin = julia_eval("os.fmin"),
       returnvalue = julia_eval("os.returnvalue"))
  # julia_command("numfields = fieldcount(typeof(os));")
  # julia_command("ostup = ntuple(i -> getfield(os, i), numfields);")
}
