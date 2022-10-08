#' Tidying methods for jglmm models
#'
#' These methods tidy the coefficients and fitted values from `jglmm` objects.
#'
#' @importFrom generics augment tidy
#' @param x An object of class `jglmm`, as returned by `jglmm()`.
#' @param ... Optional additional arguments, currently none are used.
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
tidy.jglmm <- function(x, ...) {
  julia_assign("model", x$model)

  # get fixed effect estimates
  julia_command("coef = coeftable(model);")
  julia_command("coef_df = DataFrame(term = coef.rownms,
                                     estimate = coef.cols[1],
                                     std_error = coef.cols[2],
                                     statistic = coef.cols[3],
                                     df = dof(model),
                                     p_value = coef.cols[4]);")
  fixed <- julia_eval("coef_df") |> as_tibble() |>
    rename_with(~str_replace_all(.x, "_", "\\.")) |>
    mutate(effect = "fixed", group = NA, term = coef_trans(.data$term),
           param = "beta", .before = everything())

  # get variance/covariance estimates
  # extracted code from VarCorr to get individual values
  julia_command("vc = VarCorr(model);")
  julia_command("σρ = vc.σρ;") # sd and corr for terms within groups

  # groups <- julia_eval("nmvec = string.([keys(σρ)...]);") # names of groups
  groups <- julia_eval("string.([keys(σρ)...]);") # names of groups

  # need to iterate over groups because they can have different sets of terms
  group_df <- map_df(1:length(groups), \(i) {
    group <- groups[i]
    julia_command(glue("σi = σρ[{i}].σ;")) # sds for this group
    terms <- julia_eval("string.(foldl(vcat, [keys(σi)...]))") |> str_replace(": ", "=")
    sd_terms <- julia_eval("collect(values(σi))") # sd for each term
    sd_df <- tibble(group = group, param = "sd", term = terms, estimate = sd_terms)

    corrs <- julia_eval(glue("ρi = σρ[{i}].ρ;")) # corrs for this group
    if (length(corrs) == 0) {
      corr_df <- tibble()
    } else {
      corrs <- julia_eval(glue("ρi = collect(values.(ρi));")) # corrs for this group
      n_terms <- length(terms)

      # figure out how to map corrs to terms by putting them in upper triangle
      # of a terms x terms matrix
      corr_mat <- matrix(nrow = n_terms, ncol = n_terms, dimnames = list(terms, terms))
      corr_mat[upper.tri(corr_mat)] <- corrs
      corr_df <- as_tibble(corr_mat, rownames = "term1") |>
        pivot_longer(-.data$term1, names_to = "term2", values_to = "estimate") |>
        filter(!is.na(.data$estimate)) |>
        unite("term", .data$term1, .data$term2, sep = ".") |>
        mutate(group = group, param = "cor")
    }

    bind_rows(sd_df, corr_df)
  })

  sd_resid <- julia_eval("vcs = vc.s;") # sd for residuals
  resid_df <- tibble(group = "Residual", param = "sd", term = "Observation",
                     estimate = sd_resid)

  ran_pars <- bind_rows(group_df, resid_df) |>
    mutate(effect = "ran_pars", .before = everything())

  bind_rows(fixed, ran_pars)
}

#' @rdname jglmm_tidiers
#'
#' @return `augment` returns one row for each original observation, with these
#' columns added:
#'   \item{.fitted}{predicted values}
#'   \item{.resid}{residuals}
#'
#' @export
augment.jglmm <- function(x, ...) {
  julia_assign("model", x$model)
  fits <- julia_eval("fitted(model)")
  resids <- julia_eval("residuals(model)")
  x$data |> mutate(.fitted = fits, .resid = resids)
}
