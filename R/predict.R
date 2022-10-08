#' Predictions from a model
#'
#' @importFrom stats predict
#' @param x An object of class `jglmm`, as returned by `jglmm`.
#' @param newdata (optional) A dataframe of new data.
#' @param allow.new.levels (optional) A logical indicating whether new random
#'    effects levels in \code{newdata} are allowed. If \code{FALSE} (default),
#'    such new values in \code{newdata} will trigger an error; if \code{TRUE},
#'    then the prediction will use population-level values for data with
#'    previously unobserved values.
#' @param type (optional) A character string - either \code{"link"} (default),
#'    where predictions are returned on the scale of the linear predictors, or
#'    \code{"response"}, where predictions are returned on the scale of the
#'    response variable.
#'
#' @return A numeric vector of predicted values
#' @export
#'
#' @examples
#' \dontrun{
#' jglmm_setup()
#' cbpp <- dplyr::mutate(lme4::cbpp, prop = incidence / size)
#' gm1 <- jglmm(prop ~ period + (1 | herd), data = cbpp, family = "binomial",
#'              weights = cbpp$size)
#' predict(gm1)
#' predict(gm1, type = "response")
#' newdata <- with(cbpp, expand.grid(period=unique(period), herd=unique(herd)))
#' predict(gm1, newdata)
#' }
predict.jglmm <- function(x, newdata = NULL, allow.new.levels = FALSE,
                          type = "link") {
  stopifnot(type %in% c("link", "response"))
  if(is.null(newdata)) {
    newdata <- x$data
  } else {
    resp_var <- all.vars(x$formula)[1]
    # predict in Julia requires a column corresponding to the response variable
    # initialized to a numeric value
    newdata[resp_var] <- 0
  }
  julia_assign("model", x$model)
  julia_assign("newdata", newdata)

  new_re_levels <-  if(allow.new.levels) "population" else "error"
  pred_type <-  if(type == "link") "linpred" else "response"
  is_glmm <-  julia_eval("model isa GeneralizedLinearMixedModel;")
  pred_type_string <-  if(is_glmm) glue(" , type=:{pred_type}") else ""

  fxn_call <- glue("predict(model, newdata; new_re_levels=:{new_re_levels}{pred_type_string});")
  julia_eval(fxn_call)
}

#' Simulate responses from a model
#'
#' @importFrom stats simulate
#' @param x An object of class `jglmm`, as returned by `jglmm`.
#' @param newdata (optional) A dataframe of new data.
#' @param nsim (optional) A positive integer indicating the number of responses
#'    to simulate
#'
#' @return A numeric vector of simulated values
#' @export
#'
#' @examples
#' \dontrun{
#' jglmm_setup()
#' cbpp <- dplyr::mutate(lme4::cbpp, prop = incidence / size)
#' gm1 <- jglmm(prop ~ period + (1 | herd), data = cbpp, family = "binomial",
#'              weights = cbpp$size)
#' simulate(gm1)
#' simulate(gm1, nsim = 5)
#' newdata <- with(cbpp, expand.grid(period=unique(period), herd=unique(herd)))
#' simulate(gm1, newdata)
#' }
simulate.jglmm <- function(x, newdata = NULL, nsim = 1) {
  julia_assign("model", x$model)
  if(is.null(newdata)) {
    fxn_call <- "simulate(model);"
  } else {
    resp_var <- all.vars(x$formula)[1]
    # simulate in Julia requires a column corresponding to the response variable
    # initialized to a numeric value
    newdata[resp_var] <- 0
    julia_assign("newdata", newdata)
    fxn_call <- "simulate(model, newdata);"
  }
  sims <- sapply(1:nsim, \(n) {
    julia_eval(fxn_call)
  })
  sims |> as_tibble(.name_repair = "minimal") |>
    setNames(glue("sim_{1:nsim}"))
}
