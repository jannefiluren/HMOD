# HMOD

R package with a simple snow (temperature index) and hydrological model (GR4J).

## Installation

In RStudio, install package devtools:

```R
install.packages("devtools")
```

Install the package with the following code:

```R
library(devtools)
install_github("jmgnve/NVEDATA")
```
Note that for installation on windows the package requires Rtools (https://cran.r-project.org/bin/windows/Rtools/).

## Example how to run the model

```R
iwsh <- 3

indata = list(Prec   = sample_data[[iwsh]]$Prec,
              Tair   = sample_data[[iwsh]]$Tair,
              PET    = rep(0, nrow(sample_data[[iwsh]]$Prec)),
              SWE    = matrix(0, nrow = 1, ncol = ncol(sample_data[[iwsh]]$Prec)),
              St     = matrix(0, nrow = 2, ncol = 1),
              StUH1  = matrix(0, 20, ncol = 1),
              StUH2  = matrix(0, 40, ncol = 1),
              Param  = c(74.59, 0.81, 214.98, 1.24, 3.69, 1.02))

res_sim <- model_wrapper(indata)

plot(sample_data[[iwsh]]$Runoff, type = 'l', col = 'black', lwd = 2)
lines(res_sim$Q, col = 'red')
```

## Example how to calibrate the model





































