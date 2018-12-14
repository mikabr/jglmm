.onLoad <- function(libname, pkgname) {
  JuliaCall::julia_setup()
  JuliaCall::julia_library("MixedModels")
  JuliaCall::julia_library("DataFrames")
  JuliaCall::julia_library("StatsModels")
}