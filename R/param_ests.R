#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
fixef.jglmm <- function(x) {
  julia_assign("model", x$model)
  vals <- julia_eval("fixef(model);")
  names <- julia_eval("fixefnames(model);")
  rlang::set_names(vals, names)
}

#' Extract the modes of the random effects
#'
#' Extract the conditional modes of the random effects from a fitted `jglmm`
#' object.
#'
#' @param x An object of class `jglmm`, as returned by `jglmm`.
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
