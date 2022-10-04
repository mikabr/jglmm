## code to prepare `julia_datasets` dataset goes here

# usethis::use_data(julia_datasets, overwrite = TRUE)
library(usethis)

all_datasets_name <- JuliaCall::julia_eval("MixedModels.datasets()")
all_datasets <- lapply(all_datasets_name,
               function(x){
                 JuliaCall::julia_eval(glue("DataFrame(MixedModels.dataset(:{x}))")) |> as_tibble()
               })
names(all_datasets) <- all_datasets_name


purrr::walk2(all_datasets, all_datasets_name, function(obj, name) {
  assign(name, obj)
  do.call("use_data", list(as.name(name), overwrite = TRUE))
})
