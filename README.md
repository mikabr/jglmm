# jglmm
R package for interfacing with Julia's GLMM library to fit generalized linear mixed-effects models.

## Setup
```
devtools::install_github("mikabr/jglmm")
```
Additionally, you need to have Julia installed, along with the Julia libraries `DataFrames.jl`, `StatsModels.jl`, and `MixedModels.jl`.
The location of your Julia installation needs to be known to the R package, either as the global option `JULIA_HOME` or the environmental variable `JULIA_HOME`. For example:
```
options(JULIA_HOME = "/Applications/Julia-1.2.app/Contents/Resources/julia/bin/")
```

## Usage

```
cbpp <- dplyr::mutate(lme4::cbpp, prop = incidence / size)
gm <- jglmm(prop ~ period + (1 | herd), data = cbpp, family = "binomial",
            weights = cbpp$size)
```

To set the contrasts for a categorical variable:
```
gm_effect <- jglmm(prop ~ period + (1 | herd), data = cbpp, family = "binomial",
                   weights = cbpp$size, contrasts = list(period = "effects"))
```

Access the fixed effects coefficients with `tidy(gm)` and the fitted response values with `augment(gm)`.

The available response families and their default link functions are:
```
       Bernoulli (LogitLink)
        Binomial (LogitLink)
           Gamma (InverseLink)
 InverseGaussian (InverseSquareLink)
NegativeBinomial (LogLink)
          Normal (IdentityLink)
         Poisson (LogLink)
```

Note that the first time you fit a model in a given R session it will take a while, as Julia needs to do some setup operations. Subsequent model fits will be much faster.

For more details on the underlying Julia library, see http://dmbates.github.io/MixedModels.jl/latest/index.html.
