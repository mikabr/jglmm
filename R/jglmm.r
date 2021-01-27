utils::globalVariables(c("."))

#' @importFrom dplyr "%>%"
#' @importFrom glue glue
#' @importFrom generics augment tidy
#' @importFrom JuliaCall julia_assign julia_command julia_eval
#' @importFrom rlang .data
NULL


#' Set up Julia and required libraries
#'
#' @export
jglmm_setup <- function() {
  JuliaCall::julia_setup()
  JuliaCall::julia_library("MixedModels")
  JuliaCall::julia_library("DataFrames")
  JuliaCall::julia_library("StatsModels")
}


#' Fitting Generalized Linear Mixed-Effects Models in Julia
#'
#' @param formula A two-sided linear formula object describing both the
#'   fixed-effects and random-effects part of the model, with the response on
#'   the left of a ~ operator and the terms, separated by + operators, on the
#'   right. Random-effects terms are distinguished by vertical bars ("|")
#'   separating expressions for design matrices from grouping factors.
#' @param data A data frame containing the variables named in formula.
#' @param family (optional) The distribution family for the response variable
#'   (defaults to "normal").
#' @param link (optional) The model link function (defaults to "identity").
#' @param weights (optional) A vector of prior case weights.
#' @param contrasts (optional) A named list mapping column names of categorical
#'   variables in data to coding schemes (defauls to dummy coding all
#'   categorical variables).
#'
#' @return An object of class `jglmm`.
#' @export
#'
#' @examples
#' \dontrun{
#' # linear model
#' lm1 <- jglmm(Reaction ~ Days + (Days | Subject), lme4::sleepstudy)
#'
#' # logistic model
#' cbpp <- dplyr::mutate(lme4::cbpp, prop = incidence / size)
#' gm1 <- jglmm(prop ~ period + (1 | herd), data = cbpp, family = "binomial",
#'              weights = cbpp$size)
#' gm2 <- jglmm(prop ~ period + (1 | herd), data = cbpp, family = "binomial",
#'              weights = cbpp$size, contrasts = list(period = "effects"))
#' }
jglmm <- function(formula, data, family = "normal", link = NULL, weights = NULL,
                  contrasts = NULL) {

  stopifnot(
    family %in% c("bernoulli", "binomial", "gamma", "normal", "poisson"),
    link %in% c("cauchit", "cloglog", "identity", "inverse", "logit", "log",
                "probit", "sqrt"),
    contrasts %in% c("dummy", "effects", "helmert")
  )

  # pass formula and data
  julia_assign("formula", formula)
  julia_assign("data", data)

  # construct model arguments
  model_args <- c("formula", "data")

  # choose between LinearMixedModel and GeneralizedLinearMixedModel
  if (family == "normal" & (is.null(link) || link == "identity")) {
    model_fun <- "MixedModels.LinearMixedModel"
  } else {
    model_fun <- "MixedModels.GeneralizedLinearMixedModel"
    model_args <- c(model_args, glue("{stringr::str_to_title(family)}()"))
    if (!is.null(link)) {
      model_args <- c(model_args, glue("{stringr::str_to_title(link)}Link()"))
    }
  }

  if (!is.null(contrasts)) {
    contrasts_args <- contrasts %>%
      purrr::map2_chr(names(.),
                      ~glue(":{.y} => {stringr::str_to_title(.x)}Coding()")) %>%
      paste(collapse = ", ")
    model_args <- c(model_args, glue("contrasts = Dict({contrasts_args})"))
  }

  if (!is.null(weights)) {
    julia_assign("weights", weights)
    model_args <- c(model_args, "wts = weights")
  }

  # set up and fit model
  model <- julia_eval(glue("fit({model_fun}, {paste(model_args, collapse = ', ')})"))

  results <- list(formula = formula, data = data, model = model)
  class(results) <- "jglmm"
  return(results)

}

#' Tidying methods for jglmm models
#'
#' @param x An object of class `jglmm`, as returned by `jglmm`.
#'
#' @name jglmm_tidiers
#'
#' @examples
#' \dontrun{
#' cbpp <- dplyr::mutate(lme4::cbpp, prop = incidence / size)
#' gm <- jglmm(prop ~ period + (1 | herd), data = cbpp, family = "binomial",
#'             weights = cbpp$size)
#' tidy(gm)
#' augment(gm)
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
