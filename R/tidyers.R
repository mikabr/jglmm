#' Tidying methods for jglmm models
#'
#' @param x An object of class `jglmm`, as returned by `jglmm`.
#'
#' @name jglmm_tidiers
#'
#' @examples
#' \dontrun{
#' jglmm_setup()
#' cbpp <- dplyr::mutate(lme4::cbpp, prop = incidence / size)
#' gm <- jglmm(prop ~ period + (1 | herd), data = cbpp, family = "binomial",
#'             weights = cbpp$size)
#' broom::tidy(gm)
#' broom::augment(gm)
#' }
NULL

#' @rdname jglmm_tidiers
#'
#' @return `tidy` returns a tibble of fixed effect estimates
#'
#' @export
tidy.jglmm <- function(x) {
  julia_assign("model", x$model)
  julia_command("coef = coeftable(model);")
  julia_command("coef_df = DataFrame(term = coef.rownms,
                                     estimate = coef.cols[1],
                                     std_error = coef.cols[2],
                                     statistic = coef.cols[3],
                                     df = dof(model),
                                     p_value = coef.cols[4]);")
  julia_eval("coef_df") %>% dplyr::as_tibble() %>%
    dplyr::rename_with(~stringr::str_replace_all(.x, "_", "\\.")) %>%
    dplyr::mutate(effect = "fixed") %>%
    dplyr::select(.data$effect, dplyr::everything())
}

#' @rdname jglmm_tidiers
#'
#' @return `augment` returns a tibble of the original data used to fit the model
#'   with an additional `.fitted` column containing the fitted response values.
#'
#' @export
augment.jglmm <- function(x) {
  julia_assign("model", x$model)
  fits <- julia_eval("fitted(model)")
  x$data$.fitted <- fits
  x$data %>% dplyr::as_tibble()
}