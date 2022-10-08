library(dplyr)
library(purrr)

# TODO: shouldn't set option in tests
options(JULIA_HOME = "/Applications/Julia-1.8.app/Contents/Resources/julia/bin/")
jglmm_setup()

# datasets and formulas for each model specified in:
# https://github.com/JuliaStats/MixedModels.jl/blob/main/test/modelcache.jl
test_models <- tribble(
  ~dataset,    ~formulas,
  cbpp,        list("cbpp"        = incid/hsz ~ 1 + period + (1|herd)),
  contra,      list("contra_1"    =       use ~ 1+age+abs2(age)+urban+livch+(1|urban&dist),
                    "contra_2"    =       use ~ 1+urban+(1+urban|dist)),
  grouseticks, list("grouseticks" =     ticks ~ 1+year+height+ (1|index) + (1|brood) + (1|location)),
# grouseticks, list("grouseticks" =     ticks ~ 1+year+ch+ (1|index) + (1|brood) + (1|location)),
  verbagg,     list("verbagg"     =        r2 ~ 1+anger+gender+btype+situ+(1|subj)+(1|item)),
  oxide,       list("oxide_1"     = Thickness ~ 1 + (1|Lot/Wafer),
                    "oxide_2"     = Thickness ~ 1 + Source + (1+Source|Lot) + (1+Source|Lot&Wafer)),
  dyestuff,    list("dyestuff"    =     yield ~ 1 + (1|batch)),
  dyestuff2,   list("dyestuff2"   =     yield ~ 1 + (1|batch)),
  d3,          list("d3"          =         y ~ 1 + u + (1+u|g) + (1+u|h) + (1+u|i)),
  insteval,    list("insteval_1"  =         y ~ 1 + service + (1|s) + (1|d) + (1|dept),
                    "insteval_2"  =         y ~ 1 + service*dept + (1|s) + (1|d)),
  kb07,        list("kb07_1"      =  rt_trunc ~ 1+spkr+prec+load+(1|subj)+(1|item),
                    "kb07_2"      =  rt_trunc ~ 1+spkr*prec*load+(1|subj)+(1+prec|item),
                    "kb07_3"      =  rt_trunc ~ 1+spkr*prec*load+(1+spkr+prec+load|subj)+(1+spkr+prec+load|item)),
  pastes,      list("pastes_1"    =  strength ~ 1 + (1|batch&cask),
                    "pastes_2"    =  strength ~ 1 + (1|batch/cask)),
  penicillin,  list("penicillin"  =  diameter ~ 1 + (1|plate) + (1|sample)),
  sleepstudy,  list("sleepstudy_1" = reaction ~ 1 + days + (1|subj),
                  # "sleepstudy2" =  reaction ~ 1 + days + zerocorr(1+days|subj),
                    "sleepstudy_2" = reaction ~ 1 + days + (1|subj) + (days|subj),
                    "sleepstudy_3" = reaction ~ 1 + days + (1|subj) + (0+days|subj),
                    "sleepstudy_4" = reaction ~ 1 + days + (1+days|subj)))

# fit all models from tibble with structure like test_models
fit_models <- function(models) {
  fits <- map2(models$dataset, models$formulas, function(dataset, formula_list) {
    df_fits <- map2(names(formula_list), formula_list, function(name, form) {
      message(name)
      jglmm(form, data = dataset)
    }) |> set_names(names(formula_list))
  })
}

# run fun on all fits in list as returned by fit_models
test_method <- function(fits, method) {
  map(fits, function(model_fits) {
    map2(names(model_fits), model_fits, function(name, model_fit) {
      message(name)
      method(model_fit)
    })
  })
}

test_fits <- fit_models(test_models)
