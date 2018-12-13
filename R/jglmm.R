utils::globalVariables(c("."))

#' @importFrom dplyr "%>%"
#' @importFrom glue glue
#' @import JuliaCall
#' @importFrom rlang .data
NULL


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
#' @return
#' @export
#'
#' @examples
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
  model_args <- c("formula", "data", glue("{stringr::str_to_title(family)}()"))

  if (!is.null(link)) {
    model_args <- c(model_args, glue("{stringr::str_to_title(link)}Link()"))
  }

  if (!is.null(weights)) {
    julia_assign("weights", weights)
    model_args <- c(model_args, "wt = weights")
  }

  if (!is.null(contrasts)) {
    contrasts_args <- contrasts %>%
      purrr::map2_chr(names(.),
                      ~glue(":{.y} => {stringr::str_to_title(.x)}Coding()")) %>%
      paste(collapse = ", ")
    model_args <- c(model_args, glue("contrasts = Dict({contrasts_args})"))
  }

  # set up and fit model
  julia_command(glue("model = MixedModels.GeneralizedLinearMixedModel({paste(model_args, collapse = ', ')});"))
  julia_command("fit!(model);")

  # extract coefficients
  julia_command("coef = coeftable(model);")
  julia_command("coef_df = DataFrame(coef.cols);")
  julia_command("coef_df[4] = [ coef_df[4][i].v for i in 1:length(coef_df[4]) ];")
  julia_command("names!(coef_df, [ Symbol(nm) for nm in coef.colnms ]);")
  julia_command("coef_df[:term] = coef.rownms;")
  julia_eval("coef_df") %>%
    dplyr::as_data_frame() %>%
    dplyr::select(.data$term, estimate = .data$Estimate,
                  std.error = .data$Std.Error, statistic = .data$`z value`,
                  p.value = .data$`P(>|z|)`)

}
