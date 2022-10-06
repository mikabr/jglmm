#' Tidying methods for jglmm models
#'
#' These methods tidy the coefficients and fitted values from `jglmm` objects.
#'
#' @importFrom generics augment tidy
#' @param x An object of class `jglmm`, as returned by `jglmm()`.
#'
#' @name jglmm_tidiers
#'
#' @examples
#' \dontrun{
#' jglmm_setup()
#' cbpp <- dplyr::mutate(lme4::cbpp, prop = incidence / size)
#' gm <- jglmm(prop ~ period + (1 | herd), data = cbpp, family = "binomial",
#'             weights = cbpp$size)
#' tidy(gm)
#' augment(gm)
#' }
NULL

#' @rdname jglmm_tidiers
#'
#' @return `tidy` returns a data frame with one row for each estimated effect.
#' It contains the columns:
#'   \item{effect}{\code{"fixed"} for fixed effects, \code{"ran_pars"} for random effect parameters}
#'   \item{group}{the group within which the random effect is being estimated (\code{NA} for fixed effects)}
#'   \item{param}{parameter being estimated (\code{beta} for fixed effects, \code{sd} or \code{cor} for random effect parameters)}
#'   \item{term}{term being estimated}
#'   \item{estimate}{estimated coefficient}
#'   \item{std.error}{standard error}
#'   \item{statistic}{z-statistic (\code{NA} for modes)}
#'   \item{p.value}{p-value computed from z-statistic (\code{NA} for modes)}
#'
#' @export
tidy.jglmm <- function(x) {
  julia_assign("model", x$model)

  # get fixed effect estimates
  julia_command("coef = coeftable(model);")
  julia_command("coef_df = DataFrame(term = coef.rownms,
                                     estimate = coef.cols[1],
                                     std_error = coef.cols[2],
                                     statistic = coef.cols[3],
                                     df = dof(model),
                                     p_value = coef.cols[4]);")
  fixed <- julia_eval("coef_df") |> dplyr::as_tibble() |>
    dplyr::rename_with(~stringr::str_replace_all(.x, "_", "\\.")) |>
    dplyr::mutate(effect = "fixed", group = NA, term = coef_trans(term),
                  param = "beta", .before = dplyr::everything())

  # get variance/covariance estimates
  # extracted code from VarCorr to get individual values
  julia_command("vc = VarCorr(model);")
  julia_command("σρ = vc.σρ;")
  groups <- julia_eval("nmvec = string.([keys(σρ)...]);") # names of groups
  terms <- julia_eval("cnmvec = string.(foldl(vcat, [keys(sig)...] for sig in getproperty.(values(σρ), :σ)));") # names of terms
  sd_terms <- julia_eval("σvec = vcat(collect.(values.(getproperty.(values(σρ), :σ)))...);") # sd for each term
  corr_terms <- julia_eval("collect.(values.(getproperty.(values(σρ), :ρ)));")
  sd_resid <- julia_eval("vcs = vc.s;") # sd for residuals

  terms <- terms |> stringr::str_replace(": ", "=")
  n_terms <- length(sd_terms) / length(groups)
  sd_df <- dplyr::tibble(group = rep(groups, each = n_terms), param = "sd",
                         term = terms, estimate = sd_terms)

  corr_df <- purrr::map2_df(groups, corr_terms, function(group, corrs) {
    corr_mat <- matrix(nrow = n_terms, ncol = n_terms,
                       dimnames = list(terms[1:n_terms], terms[1:n_terms]))
    corr_mat[upper.tri(corr_mat)] <- corrs
    dplyr::as_tibble(corr_mat, rownames = "term1") |>
      tidyr::pivot_longer(-term1, names_to = "term2", values_to = "estimate") |>
      dplyr::filter(!is.na(estimate)) |>
      tidyr::unite(term, term1, term2, sep = ".") |>
      dplyr::mutate(group = group, param = "cor")
  })

  ran_pars <- dplyr::bind_rows(sd_df, corr_df) |>
    dplyr::mutate(effect = "ran_pars", .before = dplyr::everything())

  # combine fixed and varcorr
  dplyr::bind_rows(fixed, ran_pars)
}

#' @rdname jglmm_tidiers
#'
#' @return `augment` returns one row for each original observation, with these
#' columns added:
#'   \item{.fitted}{predicted values}
#'   \item{.resid}{residuals}
#'
#' @export
augment.jglmm <- function(x) {
  julia_assign("model", x$model)
  fits <- julia_eval("fitted(model)")
  resids <- julia_eval("residuals(model)")
  x$data |> dplyr::mutate(.fitted = fits, .resid = resids)
}
